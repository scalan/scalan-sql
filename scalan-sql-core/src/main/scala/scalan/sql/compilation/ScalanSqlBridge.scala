package scalan.sql.compilation

import java.sql.Date

import scala.runtime.IntRef
import scalan.sql.parser.{SqlAST, SqlParser, SqlResolver}
import SqlAST._
import scala.util.Try
import scalan.sql.{ColumnUseInfo, ScalanSqlExp, SingleTableColumnUseInfo}

object ScalanSqlBridge {
  val FakeDepName = "!_fake_dep_!"
}

class ScalanSqlBridge[+S <: ScalanSqlExp](ddl: String, val scalan: S) {
  import scalan._

  protected def sqlTypeToElem(ct: ColumnType): Elem[_] = ct match {
    case IntType => IntElement
    case DoubleType => DoubleElement
    case BigIntType => LongElement
    case StringType(_, _) => StringElement
    case BoolType => BooleanElement
    case DateType => DateElement
    case TimeType => TimeElement
    case TimestampType => TimestampElement
  }

  protected lazy val parser = new SqlParser

  lazy val schema = parser.parseDDL(ddl)

  protected lazy val resolver = new SqlResolver(schema)

  protected def currentScopeName = resolver.currScope.name

  // TODO remove this, introduce fakeDeps when lowering to Iters (if necessary!)
  import ScalanSqlBridge.FakeDepName
  val fakeDepField = (FakeDepName, IntElement)

  private val tableElems: Map[String, StructElem[Struct]] = {
    schema.tables.valuesIterator.map { table =>
      val fieldElems =
        table.columns.map { case Column(colName, colType, _) => (colName, sqlTypeToElem(colType)) } :+ fakeDepField
      (table.name, structElement(fieldElems))
    }
  }.toMap

  import resolver.withContext

  // could be moved into ExprInputs, but this allows to avoid accidental reuse (see assert below)
  // Plus ExprInputs would have to be threaded through more than now
  private var reuseDetector = true

  def sqlQueryExp(query: String) = {
    assert(reuseDetector, "Instance of ScalanSqlBridge is being reused, please create a new one (based on a separate Scalan cake) for each query instead")
    reuseDetector = false
    val parsedSql = parser.parseSelect(query).operator
    val resolved = resolver.resolveOperator(parsedSql)
    val tables = allScans(resolved).toSeq.distinct

    val inputRowElems = tables.map {
      case (name, scan) =>
        // TODO include only required columns
        val eRow = tableElems.getOrElse(scan.tableName, !!!(s"Table $name not found"))
        (name, eRow)
    }

    val eInput = structElement(inputRowElems.map {
      case (name, eRow) => (name, scannableElement(eRow))
    } :+ fakeDepField)
    val resultRelationFun = inferredFun(eInput) { x =>
      val tableScopes = tables.map {
        case (name, table) => (name, field(x, name))
      }
      val scopes = tableScopes :+ (currentScopeName -> x)
      val inputs = ExprInputs(scopes: _*)
      val finalPlan: Exp[Relation[_]] = bestPlan(resolved, inputs)
      finalPlan
    }
    // could return a struct instead of adding metadate, function is easier to pass to compiler for now
    resultRelationFun.setMetadata(QueryTextKey)(query)

    resultRelationFun
  }

  def bestPlan(operator: Operator, inputs: ExprInputs) = {
    val allPlans = generateOperator(operator, inputs)
    selectBestPlan(allPlans)
  }

  // TODO plans should include cost
  // for now just assume plan which has fewest scans (and most searches) is the best
  def selectBestPlan[A](plans: Plans[A]) = plans.minBy { plan =>
    val graph = new PGraph(plan)
    val costs = graph.scheduleAll.iterator.map {
      te => te.rhs match {
        case ScannableMethods.fullScan(_, _) =>
          1000.0
        case IndexScannableMethods.search(Def(is: IndexScannable[_] @unchecked), bounds, _) =>
          // TODO check how precise bounds are
          val index = is.index.asValue
          if (index.isPrimaryKey)
            1.0
          else if (index.isUnique)
            2.0
          else
            5.0
        case _ =>
          // obviously need to be more precise
          10.0
      }
    }
    costs.sum
  }

  def scanIterName(s: Scan) = s"${s.tableName}{${s.id}}"

  def allScans(op: Any): Iterator[(String, Scan)] = op match {
    case s: Scan =>
      Iterator(scanIterName(s) -> s)
    case p: Product =>
      p.productIterator.flatMap(allScans)
    case _ => Iterator.empty
  }

  // there is at least one plan for any operator by construction, so calling `head` below should be safe
  type Plans[A] = List[Exp[Relation[A]]]

  def eRow[A](plans: Plans[A]) = plans.head.elem.eRow

  def generateOperator(op: Operator, inputs: ExprInputs): Plans[_] = ((op match {
    case s: Scan =>
      generateScan(inputs, s)
    case join: Join =>
      generateJoin(join, None, inputs)
    case Aggregate(p, groupedBy, aggregates) =>
      val inputs1 = inputs.groupByInfo(groupedBy)
      generateOperator(p, inputs1).map {
        case pExp: RRelation[a] @unchecked =>
          withContext(p) {
            generateAggregate(aggregates, groupedBy, pExp, inputs1)
          }
      }
    case Filter(p, predicate) =>
      val inputs1 = inputs.addConstraints(predicate)
      p match {
        case join: Join =>
          generateJoin(join, Some(predicate), inputs1)
        case _ =>
          val pPlans = generateOperator(p, inputs1)
          pPlans.map(generateFilter(_, p, predicate, inputs1))
      }
    case Project(p, columns) =>
      withContext(p) {
        generateOperator(p, inputs).map {
          case pExp: RRelation[a] @unchecked =>
            assert(!columns.exists(resolver.containsAggregates),
              "Aggregate columns in Project must be handled by SqlResolver")
            val structLambda = generateStructLambdaExpr(p, columns, inputs)(pExp.elem.eRow).asRep[a => Any]
            pExp.map(structLambda)
        }
      }
    case OrderBy(p, by) =>
      val inputs1 = inputs.orderByInfo(by)
      generateOperator(p, inputs1).map {
        case pExp: RRelation[a] @unchecked =>
          val eRow = pExp.elem.eRow
          val comparator = generateComparator(p, by, inputs1)(eRow)
          pExp.sort(comparator)
      }
    case TableAlias(operator, alias) =>
      generateOperator(operator, inputs)
    case SubSelect(p) =>
      generateOperator(p, inputs)
    case _ =>
      ???(s"Failed to construct Scalan graph from SQL AST $op")
  }).asInstanceOf[Plans[_]]).map(_.setMetadata(SqlOperatorKey)(op))

  def generateFilter[A](plan: RRelation[A], p: Operator, predicate: Expression, inputs: ExprInputs) = {
    val f = generateLambdaExpr(p, predicate, inputs)(plan.elem.eRow).asRep[A => Boolean]
    plan.filter(f)
  }

  private def currentLambdaArg(inputs: ExprInputs): Exp[_] = {
    def findInput(scope: resolver.Scope): Exp[_] = inputs.scopes.getOrElse(scope.name, {
      scope.outer match {
        case None =>
          !!!("Current lambda argument not found, should never happen")
        case Some(scope1) =>
          findInput(scope1)
      }
    })
    findInput(resolver.currScope)
  }

  def generateScan(inputs: ExprInputs, scan: Scan): Plans[_] = {
    def findFakeDep(x: Exp[_]): Option[Exp[Int]] = x.elem match {
      case se: StructElem[_] =>
        val x1 = x.asRep[Struct]
        if (se.fields.exists(_._1 == FakeDepName)) {
          Some(x1.get[Int](FakeDepName))
        } else {
          val relation = se.fields.iterator.map {
            case (name, _) => findFakeDep(x1.getUntyped(name))
          }.filter(_.isDefined)
          if (relation.hasNext)
            relation.next()
          else
            None
        }
      case PairElem(eFst: Elem[a], eSnd: Elem[b]) =>
        val x1 = x.asRep[(a, b)]
        findFakeDep(x1._1).orElse(findFakeDep(x1._2))
      case _ =>
        None
    }

    val currentLambdaArg = this.currentLambdaArg(inputs)
    val fakeDep = findFakeDep(currentLambdaArg).getOrElse {
      !!!(s"Can't find $FakeDepName in ${currentLambdaArg.toStringWithType}")
    }

    val tableName = scan.tableName
    val table = resolver.table(tableName)
    val eRow = tableElems(tableName)
    val scanId = scan.id

    val SingleTableColumnUseInfo(allConstraints, orderColumns, groupColumns) = inputs.columnUseInfo.forScan(scan)

    val allIndices = resolver.indices(scan.tableName)

    // TODO connect to inputs (take DB as argument?)
    val indexPlans = allIndices.flatMap { index =>
      val columns = index.columns
      val columnNames = columns.map(_.name)

      def boundsLoop(columnNames: List[String]): Option[SearchBounds] = columnNames match {
        case name :: tail =>
          allConstraints.get(name).flatMap { constraints =>
            val constraintsWithExps = constraints.flatMap {
              case (op, value) =>
                // failures just mean out-of-scope values are used and so we can't use it in a scan
                Try {
                  val valueExp = generateExpr(value, inputs)
                  (op, valueExp)
                }.toOption
            }.groupBy(_._1)

            if (constraintsWithExps.nonEmpty) {
              constraintsWithExps.get(Eq) match {
                case Some(eqConstraints) =>
                  // TODO as above, this is safe, but we should check that all values in fixed constraints are equal and that they satisfy other bounds
                  val fixedValue = eqConstraints.head._2

                  boundsLoop(tail) match {
                    case None =>
                      Some(SearchBounds.fixedValue(fixedValue))
                    case Some(bounds) =>
                      Some(bounds.addFixedValue(fixedValue))
                  }
                case None =>
                  def bound(isLower: Boolean) = {
                    def bound1(isInclusive: Boolean) = {
                      val op = (isLower, isInclusive) match {
                        case (true, true) => GreaterEq
                        case (true, false) => Greater
                        case (false, true) => LessEq
                        case (false, false) => Less
                      }

                      constraintsWithExps.get(op).map(_.map(_._2).reduce {
                        (p1, p2) =>
                          ordOp(p1, p2) {
                            (ord, elem) => if (isLower) OrderingMax(ord)(elem) else OrderingMin(ord)(elem)
                          }
                      })
                    }.map(Bound(_, isInclusive))

                    val inclusiveBound = bound1(true)
                    val exclusiveBound = bound1(false)
                    // TODO This is safe, but we can get a better bound if
                    // both are defined, using staged IF and comparison
                    exclusiveBound.orElse(inclusiveBound)
                  }

                  val lowerBound = bound(true)
                  val upperBound = bound(false)

                  Some(SearchBounds.range(lowerBound, upperBound))
              }
            } else
              None
          }
        case Nil => None
      }

      // FIXME goodForOrder/group can only be used after cost calculation is added
      // TODO common prefix is enough, we need to add partial sorting primitives
      val goodForOrder: Option[SortDirection] = None
      // orderColumns.nonEmpty && columns.map(c => (c.name, c.direction)).startsWith(orderColumns)
      val goodForGroup = false // groupColumns.nonEmpty && columnNames.takeWhile(groupColumns.contains).nonEmpty

      val iter = boundsLoop(columnNames) match {
        case Some(bounds) =>
          val scannable = IndexScannable(table, index, scanId, fakeDep)(eRow)
          val direction = goodForOrder.getOrElse(Ascending)

          Some(scannable.search(bounds, direction))
        case None =>
          val optDirection = goodForOrder.orElse(if (goodForGroup) Some(Ascending) else None)

          optDirection.map { direction =>
            IndexScannable(table, index, scanId, fakeDep)(eRow).fullScan(direction)
          }
      }

      iter.map(iterBasedRelation(_))
    }

    val tablePlan = iterBasedRelation(TableScannable(table, scanId, fakeDep)(eRow).fullScan(Ascending))

    tablePlan +: indexPlans
  }

  def generateJoin(join: Join, filter: Option[Expression], inputs: ExprInputs) = {
    join.spec match {
      case On(condition) =>
        if (join.joinType != Inner) {
          !!!("Non-inner joins are not supported yet")
        }

        val innerInputs = inputs.withoutOrderAndGroupInfo

        def joinsForOrder(leftIsOuter: Boolean) = {
          val (outer, inner) = if (leftIsOuter) (join.left, join.right) else (join.right, join.left)

          (generateOperator(outer, inputs), bestPlan(inner, innerInputs)) match {
            case (outerPlans: Plans[a] @unchecked, innerPlan: RRelation[b] @unchecked) =>
              val outerRowElem = eRow(outerPlans)
              val innerRowElem = innerPlan.elem.eRow

              val makeHashJoin: RRelation[a] => RRelation[_] = {
                val (outerJoinColumns, innerJoinColumns) =
                  withContext(outer) {
                    val OuterScopeName = currentScopeName
                    withContext(inner) {
                      val InnerScopeName = currentScopeName

                      // TODO this should return Option; if None, hashJoin can't be used
                      def columns(cond: Expression): List[(ResolvedAttribute, ResolvedAttribute)] = cond match {
                        case BinOpExpr(SqlAST.And, l, r) =>
                          columns(l) ++ columns(r)
                        case BinOpExpr(Eq, l: ResolvedAttribute, r: ResolvedAttribute) =>
                          val bindingL = resolver.lookup(l)
                          val bindingR = resolver.lookup(r)
                          bindingL.scope match {
                            case OuterScopeName =>
                              if (bindingR.scope == InnerScopeName)
                                List((l, r))
                              else
                                !!!(s"$l and $r must be columns on opposing join sides", (outerPlans :+ innerPlan): _*)
                            case InnerScopeName =>
                              if (bindingR.scope == OuterScopeName)
                                List((r, l))
                              else
                                !!!(s"$l and $r must be columns on opposing join sides", (outerPlans :+ innerPlan): _*)
                            case _ =>
                              !!!(s"$l seems not to be a column of $outer or $inner: binding $bindingL", (outerPlans :+ innerPlan): _*)
                          }
                        case _ =>
                          !!!(s"Unsupported join condition: $cond", (outerPlans :+ innerPlan): _*)
                      }

                      columns(condition).unzip
                    }
                  }

                def keyFun[A](elem: Elem[A], op: Operator, columns: Seq[ResolvedAttribute]) = inferredFun(elem) { x =>
                  withContext(op) {
                    def column(column: ResolvedAttribute) =
                      ExprInputs(currentScopeName -> x).resolveColumn(column)

                    columns match {
                      case Seq() =>
                        !!!(s"Join using empty column list", (outerPlans :+ innerPlan): _*)
                      case Seq(col) =>
                        column(col)
                      case _ =>
                        val fields = columns.map(column)
                        tupleStruct(fields: _*)
                    }
                  }
                }

                val outerKeyFun = keyFun(outerRowElem, outer, outerJoinColumns)
                val innerKeyFun = keyFun(innerRowElem, inner, innerJoinColumns)
                if (outerKeyFun.elem.eRange != innerKeyFun.elem.eRange) {
                  !!!(s"Different key types for two join sides: ${outerKeyFun.elem.eRange} and ${innerKeyFun.elem.eRange}",
                    (outerPlans :+ innerPlan :+ outerKeyFun :+ innerKeyFun): _*)
                }

                if (leftIsOuter)
                  _.hashJoin(innerPlan, outerKeyFun, innerKeyFun, leftIsOuter)
                else
                  innerPlan.hashJoin(_, innerKeyFun, outerKeyFun, leftIsOuter)
              }

              val flatMapArg = withContext(outer) {
                inferredFun(outerRowElem) { x =>
                  val condition1 = filter match {
                    case None => condition
                    case Some(pred) => BinOpExpr(SqlAST.And, condition, pred)
                  }
                  val inner1 = resolver.pushdownFilter(inner, condition1)

                  val inner1Plan = bestPlan(inner1, innerInputs + (currentScopeName -> x)).asRep[Relation[b]]

                  val g = inferredFun(innerRowElem) { y =>
                    if (leftIsOuter) (x, y) else (y, x)
                  }

                  inner1Plan.map(g)
                }
              }

              outerPlans.flatMap { outerPlan =>
                val hashJoinUnfiltered = makeHashJoin(outerPlan)
                val hashJoin = filter match {
                  case None => hashJoinUnfiltered
                  case Some(pred) =>
                    generateFilter(hashJoinUnfiltered, join, pred, inputs)
                }

                val nestedLoopJoin = outerPlan.flatMap(flatMapArg)
                Iterator[RRelation[_]](hashJoin, nestedLoopJoin)
              }
          }
        }

        joinsForOrder(leftIsOuter = true) ++ joinsForOrder(leftIsOuter = false)
      case otherSpec =>
        !!!(s"Join spec $otherSpec after resolver")
    }
  }

  def generateAggregate[Row](aggregates: List[AggregateExpr], groupedBy: List[Expression], pExp: Exp[Relation[Row]], inputs: ExprInputs) = {
    def aggOperand(agg: AggregateExpr, inputs: ExprInputs): Exp[_] =
      agg.op match {
        case Count =>
          1
        case _ =>
          generateExpr(agg.value, inputs)
      }

    def aggCombine[A](agg: AggregateExpr, x: Exp[A], y: Exp[A])(implicit elem: Elem[A]): Exp[A] = agg.op match {
      case Count | Avg | Sum =>
        implicit val num = getNumeric(elem)
        x + y
      case Max =>
        implicit val ord = getOrdering(elem)
        x.max(y)
      case Min =>
        implicit val ord = getOrdering(elem)
        x.min(y)
    }

    def aggElem(agg: AggregateExpr): Elem[_] = agg.op match {
      case Count => IntElement
      case _ => getExprType(agg.value)
    }

    // casts of Elem[_] to TypeDesc before matching are workarounds for https://issues.scala-lang.org/browse/SI-9779
    def aggInitialValue(agg: AggregateExpr): Exp[_] =
      agg.op match {
        case Count =>
          0
        case Sum | Avg =>
          getExprType(agg.value) match {
            case elem: Elem[a] =>
              val num = getNumeric(elem)
              toRep(num.zero)(elem)
          }
        case Max =>
          getExprType(agg.value).asInstanceOf[TypeDesc] match {
            case IntElement => Int.MinValue
            case LongElement => Long.MinValue
            case FloatElement => Float.NegativeInfinity
            case DoubleElement => Double.NegativeInfinity
            case elem =>
              !!!(s"No minimum value for $elem")
          }
        case Min =>
          getExprType(agg.value).asInstanceOf[TypeDesc] match {
            case IntElement => Int.MaxValue
            case LongElement => Long.MaxValue
            case FloatElement => Float.NegativeInfinity
            case DoubleElement => Double.NegativeInfinity
            case elem =>
              !!!(s"No maximum value for $elem")
          }
      }

    implicit val eRow = pExp.elem.eRow

    if (aggregates.exists(_.distinct)) {
      !!!(s"Distinct aggregates are not supported yet", pExp)
    }

    val eV0 = structElement(aggregates.zipWithIndex.map { case (agg, i) => tupleFN(i) -> aggElem(agg)})
    eV0.asInstanceOf[Elem[_]] match {
      case eV: Elem[v] =>
        implicit val eV0 = eV

        val newValue = thunk_create { tupleStruct(aggregates.map(aggInitialValue): _*).asRep[v] }
        val reduce = fun[(v, Row), v] {
          case Pair(x, y) =>
            val inputs1 = inputs + (currentScopeName -> y)
            eV match {
              case se: StructElem[_] =>
                val resFields = aggregates.zip(se.fields).map {
                  case (agg, (name, eF: Elem[f])) =>
                    val x1 = field(x.asRep[Struct], name).asRep[f]
                    val y1 = aggOperand(agg, inputs1).asRep[f]
                    (name, aggCombine(agg, x1, y1)(eF))
                }
                struct(se.structTag.asInstanceOf[StructTag[Struct]], resFields).asRep[v]
              case _ =>
                aggCombine(aggregates(0), x.asRep[v], y.asRep[v])(eV)
            }
        }

        if (groupedBy.nonEmpty) {
          val mapKey = inferredFun(eRow) { x =>
            val inputs1 = inputs + (currentScopeName -> x)
            val byExps = groupedBy.map(generateExpr(_, inputs1))
            // TODO optimize case of a single expression (commented out below, in mapValue as well)
            // as part of Slicing or a separate pass?
            tupleStruct(byExps: _*)
            //              byExps match {
            //                case List(byExp) =>
            //                  byExp
            //                case _ =>
            //                  tupleStruct(byExps: _*)
            //              }
          }

          pExp.mapReduce(mapKey, newValue, reduce)
        } else {
          pExp.reduce(reduce, newValue)
        }
    }
  }

  def generateComparator[A](table: Operator, order: List[SortSpec], inputs: ExprInputs)(implicit eRow: Elem[A]) = {
    def compare(l: Exp[_], r: Exp[_]) = {
      widen(l, r) match {
        case lrhs: LRHS[a] =>
          val elem = lrhs.elem
          val ord = getOrdering(elem)
          OrderingLT(ord)(lrhs.l, lrhs.r)
      }
    }

    withContext(table) {
      fun[(A, A), Boolean] { x =>
        // TODO NullsOrdering currently ignored
        order.map {
          case SortSpec(expr, direction, _) =>
            val lhs = generateExpr(expr, inputs + (currentScopeName -> x._1))
            val rhs = generateExpr(expr, inputs + (currentScopeName -> x._2))
            direction match {
              case Ascending =>
                (compare(lhs, rhs), lhs === rhs)
              case Descending =>
                (compare(rhs, lhs), rhs === lhs)
            }
        }.foldRight(toRep(false)) { (comp1, acc) =>
          comp1._1 || (comp1._2 && acc)
        }
      }
    }
  }

  def generateLambdaExpr[A](table: Operator, exp: Expression, inputs: ExprInputs)(eIn: Elem[A]): Exp[A => _] = {
    withContext(table) {
      inferredFun(eIn) { x =>
        generateExpr(exp, inputs + (currentScopeName -> x))
      }
    }
  }

  def generateStructLambdaExpr[A](table: Operator, exps: List[ProjectionColumn], inputs: ExprInputs)(eIn: Elem[A]): Exp[A => _] = {
    withContext(table) {
      inferredFun(eIn) { x =>
        val inputs1 = inputs + (currentScopeName -> x)
        val unnamedCounter = new IntRef(0)

        val fields = exps.map { exp =>
          val name = this.name(exp, unnamedCounter)
          val value = generateExpr(exp.expr, inputs1)
          (name, value)
        }
        struct(fields)
      }
    }
  }

  // TODO move `name` up to Expression, remove this
  // or just construct usable names here?
  def name(column: ProjectionColumn, unnamedCounter: IntRef) =
    column.alias.getOrElse {
      def name(expr: Expression): String = expr match {
        case c: ResolvedTableAttribute =>
          c.name
        case ResolvedProjectedAttribute(expr1, alias, _) =>
          alias.getOrElse(name(expr1))
        case _ =>
          unnamedCounter.elem += 1
          s"_${unnamedCounter.elem}"
      }

      name(column.expr)
    }

  def getExprType(expr: Expression): Elem[_] = {
    sqlTypeToElem(resolver.getExprType(expr))
  }

  private def getOrdering[T](e: Elem[T]): Ordering[T] = (e.asInstanceOf[TypeDesc] match {
    case IntElement => implicitly[Ordering[Int]]
    case LongElement => implicitly[Ordering[Long]]
    case DoubleElement => implicitly[Ordering[Double]]
    case CharElement => implicitly[Ordering[Char]]
    case StringElement => implicitly[Ordering[String]]
    case DateElement => implicitly[Ordering[Date]]
    case PairElem(eFst: Elem[a], eSnd: Elem[b]) =>
      val ordA: Ordering[a] = getOrdering(eFst)
      val ordB: Ordering[b] = getOrdering(eSnd)
      Ordering.Tuple2(ordA, ordB)
    case _ => ???(s"Don't know how to create Ordering for $e")
  }).asInstanceOf[Ordering[T]]

  private def getNumeric[T](e: Elem[T]): Numeric[T] = (e.asInstanceOf[TypeDesc] match {
    case IntElement => implicitly[Numeric[Int]]
    case DoubleElement => implicitly[Numeric[Double]]
    case LongElement => implicitly[Numeric[Long]]
    case BooleanElement => BooleanNumeric
//    case DateElement => implicitly[Numeric[Date]]
    case _ => ???(s"Don't know how to create Numeric for $e")
  }).asInstanceOf[Numeric[T]]

  private case class LRHS[A](l: Exp[A], r: Exp[A], elem: Elem[A])

  // brings l and r to the same type
  private def widen[A, B](l: Exp[A], r: Exp[B]): LRHS[_] = {
    implicit val eL = l.elem
    implicit val eR = r.elem
    if (eL == eR)
      LRHS(l, r.asRep[A], eL)
    else
      (eL.asInstanceOf[TypeDesc], eR.asInstanceOf[TypeDesc]) match {
        // Should handle two structs with same field names?
        case (StructElem(_, fields), _) =>
          if (fields.length == 1) {
            val l1 = field(l.asRep[Struct], 0)
            widen(l1, r)
          } else {
            !!!(s"Arithmetic operation on a multi-field struct ${l.toStringWithType}", l, r)
          }
        case (_, StructElem(_, fields)) =>
          if (fields.length == 1) {
            val r1 = field(r.asRep[Struct], 0)
            widen(l, r1)
          } else {
            !!!(s"Arithmetic operation on a multi-field struct ${r.toStringWithType}", l, r)
          }
        // zipWith for two iterators?
        case (eL: RelationElem[a, _], _) =>
          // TODO verify that l itself isn't used elsewhere, same below
          val l1 = l.asRep[Relation[a]].onlyValue()
          widen(l1, r)
        case (_, eR: RelationElem[b, _]) =>
          val r1 = r.asRep[Relation[b]].onlyValue()
          widen(l, r1)
        case (DoubleElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Double], r.toDouble, DoubleElement)
        case (_, DoubleElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toDouble, r.asRep[Double], DoubleElement)
        case (LongElement, FloatElement) =>
          LRHS(l.asRep[Long].toDouble, r.asRep[Float].toDouble, DoubleElement)
        case (FloatElement, LongElement) =>
          LRHS(l.asRep[Float].toDouble, r.asRep[Long].toDouble, DoubleElement)
        case (LongElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Long], r.toLong, LongElement)
        case (_, LongElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toLong, r.asRep[Long], LongElement)
        case (FloatElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Float], r.toFloat, FloatElement)
        case (_, FloatElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toFloat, r.asRep[Float], FloatElement)
        case (IntElement, _) =>
          implicit val numR = getNumeric(eR)
          LRHS(l.asRep[Int], r.toInt, IntElement)
        case (_, IntElement) =>
          implicit val numL = getNumeric(eL)
          LRHS(l.toInt, r.asRep[Int], IntElement)
        // Dates and times are represented as strings in SQLite
        case (DateElement | TimeElement | TimestampElement, StringElement) =>
          LRHS(l.asRep[String], r.asRep[String], StringElement)
        case (StringElement, DateElement | TimeElement | TimestampElement) =>
          LRHS(l.asRep[String], r.asRep[String], StringElement)
        case (StringElement, CharElement) =>
          LRHS(l.asRep[String], r.toStringRep, StringElement)
        case (CharElement, StringElement) =>
          LRHS(l.toStringRep, r.asRep[String], StringElement)
        case _ =>
          !!!(s"No common numeric type for ${l.toStringWithType} and ${r.toStringWithType}", l, r)
      }
  }

  private def numOp[A, B](l: Exp[A], r: Exp[B])(f: (Numeric[A], Elem[A]) => BinOp[A, A]) = {
    val LRHS(l1, r1, elem) = widen(l, r)
    val num = getNumeric(elem)
    // works by type erasure
    f(num.asInstanceOf[Numeric[A]], elem.asElem[A])(l1.asRep[A], r1.asRep[A])
  }

  private def ordOp[A, B, C](l: Exp[A], r: Exp[B])(f: (Ordering[A], Elem[A]) => BinOp[A, C]) = {
    val LRHS(l1, r1, elem) = widen(l, r)
    val ord = getOrdering(elem)
    f(ord.asInstanceOf[Ordering[A]], elem.asElem[A])(l1.asRep[A], r1.asRep[A])
  }

  case class ExprInputs(scopes: Map[String, Exp[_]], columnUseInfo: ColumnUseInfo) {
    def addConstraints(predicate: Expression) = {
      val clauses = resolver.conjunctiveClauses(predicate)
      val extractedConstraints = clauses.flatMap {
        case clause @ BinOpExpr(op: ComparisonOp, l, r) =>
          (resolver.underlyingTableColumn(l), resolver.underlyingTableColumn(r)) match {
            case (None, None) =>
              Nil
            case (Some(l1), None) =>
              List((l1, (op, r)))
            case (None, Some(r1)) =>
              List((r1, (op.inverse, l)))
            case (Some(l1), Some(r1)) =>
              List(
                (l1, (op, r1)),
                (r1, (op.inverse, l1))
              )
          }
        case _ =>
          Nil
      }

      val newConstraints = extractedConstraints.foldLeft(columnUseInfo.constraints) {
        case (map, (attr, ct)) =>
          val currentConstraints = map.getOrElse(attr, Set.empty)
          map.updated(attr, currentConstraints + ct)
      }

      copy(columnUseInfo = columnUseInfo.copy(constraints = newConstraints))
    }

    def orderByInfo(sortSpecs: List[SortSpec]) = {
      val orderBy = sortSpecs.map {
        case SortSpec(expr, direction, _) =>
          (resolver.underlyingTableColumn(expr), direction)
      }.takeWhile(_._1.isDefined).map { case (opt, direction) => (opt.get, direction) }
      copy(columnUseInfo = columnUseInfo.copy(orderBy = orderBy))
    }

    def groupByInfo(expressions: List[Expression]) = {
      val tableAttributes = expressions.flatMap(resolver.underlyingTableColumn).toSet
      copy(columnUseInfo = columnUseInfo.copy(groupBy = tableAttributes))
    }

    def withoutOrderAndGroupInfo =
      copy(columnUseInfo = columnUseInfo.copy(orderBy = Nil, groupBy = Set.empty))

    // TODO inline
    def resolveColumn(c: ResolvedAttribute) = {
      val binding = resolver.lookup(c)
      scopes.get(binding.scope) match {
        case None =>
          !!!(s"Failed to resolve $c. Binding is $binding.")
        case Some(x) =>
          binding.path.foldLeft[Exp[_]](x) {
            case (y, resolver.First) =>
              y.elem match {
                case pe: PairElem[a, b] =>
                  y.asRep[(a, b)]._1
              }
            case (y, resolver.Second) =>
              y.elem match {
                case pe: PairElem[a, b] =>
                  y.asRep[(a, b)]._2
              }
            case (y, resolver.Field(fieldName)) =>
              field(y.asRep[Struct], fieldName)
            case (y, resolver.Index(i)) =>
              y.elem match {
                case se: StructElem[_] =>
                  val fieldName = se.fieldNames(i)
                  field(y.asRep[Struct], fieldName)
              }
          }
      }
    }

    def +(pair: (String, Exp[_])) = copy(scopes = scopes + pair)

    def apply(name: String) = scopes(name)
  }

  object ExprInputs {
    def apply(scopes: (String, Exp[_])*): ExprInputs = new ExprInputs(scopes.toMap, ColumnUseInfo())
  }

def generateExpr(expr: Expression, inputs: ExprInputs): Exp[_] = ((expr match {
    case agg: AggregateExpr =>
      !!!(s"generateExpr called on aggregate expression $agg, all aggregate expressions are handled separately")
    case c: UnresolvedAttribute =>
      !!!(s"Unresolved attribute $c survived SqlResolver")
    case resolved: ResolvedAttribute =>
      inputs.resolveColumn(resolved)
    case BinOpExpr(op, l, r) =>
      val lExp = generateExpr(l, inputs)
      val rExp = generateExpr(r, inputs)
      // TODO null handling
      op match {
        case SqlAST.And =>
          lExp.asRep[Boolean] && rExp.asRep[Boolean]
        case SqlAST.Or =>
          lExp.asRep[Boolean] || rExp.asRep[Boolean]
        case Concat =>
          lExp.asRep[String] + rExp.asRep[String]
        case Plus =>
          numOp(lExp, rExp)(NumericPlus(_)(_))
        case Minus =>
          numOp(lExp, rExp)(NumericMinus(_)(_))
        case Times =>
          numOp(lExp, rExp)(NumericTimes(_)(_))
        case Divide =>
          numOp(lExp, rExp)((num, elem) => num match {
            case i: Integral[_] => IntegralDivide(i)(elem)
            case f: Fractional[_] => FractionalDivide(f)(elem)
          })
        case Modulo =>
          numOp(lExp, rExp)((num, elem) => num match {
            case i: Integral[_] => IntegralMod(i)(elem)
            case _ =>
              !!!(s"Modulo on non-integral type ${elem.name}")
          })
        case Eq | Is =>
          ordOp(lExp, rExp)((_, _) => Equals())
        case Less =>
          ordOp(lExp, rExp)((ord, _) => OrderingLT(ord))
        case LessEq =>
          ordOp(lExp, rExp)((ord, _) => OrderingLTEQ(ord))
        case Greater =>
          ordOp(lExp, rExp)((ord, _) => OrderingGT(ord))
        case GreaterEq =>
          ordOp(lExp, rExp)((ord, _) => OrderingGTEQ(ord))
      }
    case NotExpr(opd) =>
      !generateExpr(opd, inputs).asRep[Boolean]
    case NegExpr(opd) =>
      generateExpr(opd, inputs) match {
        case opdExp: Exp[a] =>
          implicit val opdElem = opdExp.elem
          implicit val num = getNumeric(opdElem)
          - opdExp
      }
    case ExistsExpr(q) =>
      bestPlan(q, inputs) match {
        case qExp: RRelation[a] @unchecked =>
          !qExp.isEmpty
      }
    case LikeExpr(l, r, escape) =>
      patternMatch(l, r, escape, inputs)
    case l @ Literal(v, t) =>
      val elem = sqlTypeToElem(t)
      // non-null literals in queries are replaced by parameters
      val currentLambdaArg = this.currentLambdaArg(inputs)
      l.index match {
        case Some(i) =>
          Parameter(i, currentLambdaArg, v)(elem)
        case None =>
          toRep(v)(elem.asElem[Any])
      }

    case NullLiteral =>
      !!!("Nulls aren't supported")
    // if we ever support null, how to determine type here?
    // toRep(null: Any)(AnyElement)
    case CastExpr(exp, typ) =>
      generateExpr(exp, inputs) match {
        case exprExp: Exp[a] =>
          implicit val elem = exprExp.elem
          typ match {
            case StringType(_, _) =>
              exprExp.toStringRep
            case DoubleType =>
              implicit val num = getNumeric(elem)
              exprExp.toDouble
            case IntType =>
              implicit val num = getNumeric(elem)
              exprExp.toInt
          }
      }
    case SelectExpr(s) =>
      bestPlan(s, inputs)
    case SubstrExpr(str, from, len) =>
      val strExp = generateExpr(str, inputs).asRep[String]
      val fromExp = generateExpr(from, inputs).asRep[Int]
      val lenExp = generateExpr(len, inputs).asRep[Int]
      strExp.substring(fromExp, fromExp + lenExp)
    case CaseWhenExpr(cases) =>
      generateCaseWhen(cases, inputs)
    case InListExpr(expr, list) =>
      val exprExp = generateExpr(expr, inputs)
      list.map(x => exprExp === generateExpr(x, inputs)).reduce(_ || _)
    case InExpr(expr, query) =>
      val exprExp = generateExpr(expr, inputs)
      bestPlan(query, inputs) match {
        case queryExp: RRelation[a] @unchecked =>
          val f = inferredFun(queryExp.elem.eRow) { _.asRep[Any] === exprExp }
          !queryExp.filter(f).isEmpty
      }
    case FuncExpr(name, args) =>
      val argExps = args.map(generateExpr(_, inputs))
      try {
        funcExpr(name, argExps)
      } catch {
        case e: Exception =>
          !!!(s"Failure to generate Scalan Exp for $name(${argExps.mkString(", ")})", e, argExps: _*)
      }
    case _ => throw new NotImplementedError(s"generateExpr($expr)")
  }): Exp[_]).setMetadata(SqlExpressionKey)(expr)

  def funcExpr(name: String, argExps: List[Exp[_]]): Exp[_] = name match {
    case "strftime" =>
      // see https://www.sqlite.org/lang_datefunc.html
      // current support is very incomplete
      argExps(0) match {
        case Def(Parameter(_, _, format: String)) =>
          val timeString = argExps(1).asRep[String]
          format match {
            case "%Y" =>
              timeString.substring(0, 4)
            case "%m" =>
              timeString.substring(5, 7)
            case "%d" =>
              timeString.substring(8, 10)
            case _ =>
              s"Currrently unsupported format string for strftime: $format"
          }
        case s =>
          !!!(s"First argument to strftime is expected to be a string literal, but was ${s.toStringWithDefinition}")
      }
    case _ =>
      !!!(s"Don't know how to generate code for function $name", argExps: _*)
  }

  // TODO escape properly
  def patternMatch(text: Expression, pattern: Expression, escape: Option[Expression], inputs: ExprInputs) = {
    val left = generateExpr(text, inputs).asRep[String]
    if (escape.isDefined) {
      !!!("ESCAPE clause isn't supported", left)
    }
    pattern match {
      case Literal(v, StringType(_, _)) =>
        val p = v.toString
        if (p.indexOf('%') < 0 && p.indexOf('_') < 0)
          left === p
        else if (p.lastIndexOf('%') == 0 && p.indexOf('_') < 0)
          left.endsWith(p.substring(1))
        else if (p.indexOf('%') == p.length - 1 && p.indexOf('_') < 0)
          left.startsWith(p.substring(0, p.length - 1))
        else if (p.lastIndexOf('%', p.length - 2) == 0 && p.indexOf('%', 1) == p.length - 1 && p.indexOf('_') < 0)
          left.contains(p.substring(1, p.length - 1))
        else
          left.matches(p.replace("%", ".*").replace('_', '.'))
      case _ =>
        !!!(s"Pattern in LikeExpr must be a string literal, but was $pattern")
    }
  }

  def generateCaseWhen(list: ExprList, inputs: ExprInputs): Exp[_] = list match {
    case Nil =>
      !!!("CaseWhen: wrong number of expressions")
    case e :: Nil =>
      // last else clause
      generateExpr(e, inputs)
    case cond :: t :: e =>
      IF (generateExpr(cond, inputs).asRep[Boolean]) THEN {
        generateExpr(t, inputs).asRep[Any]
      } ELSE
        generateCaseWhen(e, inputs).asRep[Any]
  }

  // FIXME SqlCompiler.and doesn't check the literal's value
}
