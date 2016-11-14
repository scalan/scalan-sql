package scalan.sql.compilation

import scala.runtime.IntRef
import scalan.sql.parser.{SqlAST, SqlParser, SqlResolver}
import SqlAST._
import scala.annotation.tailrec
import scala.util.Try
import scalan.sql.{ColumnUseInfo, ConstraintSet, ScalanSqlExp, SingleTableColumnUseInfo}

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

  import resolver.withContext

  // could be moved into ExprInputs, but this allows to avoid accidental reuse (see assert below)
  // Plus ExprInputs would have to be threaded through more than now
  private var reuseDetector = true

  // TODO should default for turnLiteralsIntoParameters be false? Or read from config?
  def sqlQueryExp(query: String, turnLiteralsIntoParameters: Boolean = true) = {
    assert(reuseDetector, "Instance of ScalanSqlBridge is being reused, please create a new one (based on a separate Scalan cake) for each query instead")
    reuseDetector = false
    val parsedSql = parser.parseSelect(query, turnLiteralsIntoParameters).operator
    val resolved = resolver.resolveOperator(parsedSql)

    val scanElems = allUsedAttributes(resolved).toSeq.groupBy(tableAttr => Scan(tableAttr.table.name, tableAttr.tableId)).map {
      case (scan, attrs) =>
        val attrsInOrder = attrs.distinct.sortBy(_.index)
        val fields = attrsInOrder.map(a => (a.name, sqlTypeToElem(a.sqlType)))
        (scan, structElement(fields))
    }

    val eInput = kernelInputElement

    val resultRelationFun = inferredFun(eInput) { x =>
      val inputs = ExprInputs(x, scanElems, currentScopeName -> x)
      val finalPlan = bestPlan(resolved, inputs)
      val finalNode: Exp[Relation[_]] = finalPlan.node
      finalNode
    }
    // could return a struct instead of adding metadate, function is easier to pass to compiler for now
    resultRelationFun.setMetadata(QueryTextKey)(query)

    resultRelationFun
  }

  // finds all columns used in the expressions inside
  def allUsedAttributes(op: Any): Iterator[ResolvedTableAttribute] = op match {
    case rta: ResolvedTableAttribute => Iterator(rta)
    case rpa: ResolvedProjectedAttribute => allUsedAttributes(rpa.parent)
    case p: Product => p.productIterator.flatMap(allUsedAttributes)
    case _ => Iterator.empty
  }

  def bestPlan(operator: Operator, inputs: ExprInputs) = {
    val allPlans = generateOperator(operator, inputs)
    selectBestPlan(allPlans)
  }

  def selectBestPlan[A](plans: Plans[A]) = plans.minBy(_.cost)

  case class Plan[A](node: Exp[Relation[A]], constraints: ConstraintSet, ordering: SqlOrdering) {
    def elem = node.elem
    // TODO Fix cost calculation
    // for now just assume plan which has fewest scans (and most searches) is the best
    lazy val cost = {
      val graph = new PGraph(node)
      val costs = graph.scheduleAll.iterator.map {
        te => te.rhs match {
          case ScannableMethods.fullScan(_) =>
            1000.0
          case IndexScannableMethods.search(Def(is: IndexScannable[_] @unchecked), bounds) =>
            // TODO check how precise bounds are
            val index = is.index.asValue
            if (index.isPrimaryKey || is.isCovering)
              1.0
            else if (index.isUnique)
              2.0
            else
              5.0
          case RelationMethods.sort(_, _) | RelationMethods.sortBy(_, _) =>
            10000.0
          case RelationMethods.partialSort(_, _, _) =>
            5000.0
          case RelationMethods.partialMapReduce(_, _, _, _, _) =>
            50.0
          case _ =>
            // obviously need to be more precise
            100.0
        }
      }
      costs.sum
    }
  }
  // there is at least one plan for any operator by construction, so calling `head` below should be safe
  type Plans[A] = List[Plan[A]]

  def eRow[A](plans: Plans[A]) = plans.head.elem.eRow

  def generateOperator(op: Operator, inputs: ExprInputs): Plans[_] = ((op match {
    case s: Scan =>
      generateScan(inputs, s)
    case join: Join =>
      generateJoin(join, None, inputs)
    case Aggregate(p, groupedBy, aggregates) =>
      val inputs1 = inputs.groupByInfo(groupedBy)
      generateOperator(p, inputs1).map {
        case pExp: Plan[a] @unchecked =>
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
          generateOperator(p, inputs1) match {
            case pPlans: Plans[a] @unchecked =>
              (pPlans: Plans[a]).map(generateFilter(_, p, predicate, inputs1))
          }
      }
    case Project(p, columns) =>
      withContext(p) {
        generateOperator(p, inputs).map {
          case pExp: Plan[a] @unchecked =>
            // can't contain unresolved star, if it does will get a ClassCastException
            val columns1 = columns.asInstanceOf[List[ProjectionColumn]]
            assert(!columns1.exists(resolver.containsAggregates),
              "Aggregate columns in Project must be handled by SqlResolver")
            val structLambda = generateStructLambdaExpr(p, columns1, inputs)(pExp.elem.eRow).asRep[a => Any]
            Plan(pExp.node.map(structLambda), pExp.constraints, pExp.ordering)
        }
      }
    case OrderBy(p, by) =>
      val inputs1 = inputs.orderByInfo(by)

      generateOperator(p, inputs1).map {
        case pPlan: Plan[a]@unchecked =>

          @tailrec
          def findCommonPrefix[A](knownOrdering: SqlOrdering, required: List[A], foundMatches: List[A])(compare: (SortSpec, A) => Boolean): (List[A], List[A]) =
            (knownOrdering, required) match {
              case (gHead :: gTail, dHead :: dTail) =>
                if (compare(gHead, dHead)) {
                  findCommonPrefix(gTail, dTail, dHead :: foundMatches)(compare)
                } else {
                  (foundMatches, required)
                }
              case _ => (foundMatches, required)
            }

          val eRow = pPlan.elem.eRow
          val (prefix, suffix) = findCommonPrefix(pPlan.ordering, by, Nil) {
            case (SortSpec(gHeadCol, gHeadDir, gNulls), SortSpec(dHeadCol, dHeadDir, dNulls)) =>
              gHeadDir == dHeadDir && gNulls == dNulls &&
                (dHeadCol == gHeadCol || underlyingTableColumn(dHeadCol) == Some(gHeadCol) || underlyingTableColumn(gHeadCol) == Some(dHeadCol))
          }
          if (suffix.isEmpty) {
            // already sorted in desired order
            pPlan
          } else {
            val comparator = generateComparator(p, suffix, inputs1)(eRow)
            val sortedNode = if (prefix.isEmpty) {
              pPlan.node.sort(comparator)
            } else {
              val prefixComparator = generateEquality(p, prefix.map(_.expr), inputs1)(eRow)
              pPlan.node.partialSort(prefixComparator, comparator)
            }
            Plan(sortedNode, pPlan.constraints, prefix reverse_::: suffix)
          }
      }
    case TableAlias(operator, alias) =>
      generateOperator(operator, inputs)
    case SubSelect(p) =>
      generateOperator(p, inputs)
    case _ =>
      ???(s"Failed to construct Scalan graph from SQL AST $op")
  }).asInstanceOf[Plans[_]]).map { plan =>
    plan.node.setMetadata(SqlOperatorKey)(op)
    plan
  }

  def generateFilter[A](plan: Plan[A], p: Operator, predicate: Expression, inputs: ExprInputs) = {
    plan.constraints.simplify(predicate) match {
      case Some(simplified) =>
        val f = generateLambdaExpr(p, simplified, inputs)(plan.elem.eRow).asRep[A => Boolean]
        Plan(plan.node.filter(f), plan.constraints.addConstraints(simplified), plan.ordering)
      case None =>
        plan
    }
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
    val currentLambdaArg = this.currentLambdaArg(inputs)
    val fakeDep = extraDeps(currentLambdaArg)

    val tableName = scan.tableName
    val table = resolver.table(tableName)
    inputs.scanElems(scan) match {
      case eRow: Elem[a] =>
        val scanId = scan.id

        val SingleTableColumnUseInfo(allConstraints, orderColumns, groupColumns) = inputs.columnUseInfo.forScan(scan)

        val allIndices = resolver.indices(scan.tableName)

        // TODO connect to inputs (take DB as argument?)
        val indexPlans = allIndices.flatMap { index =>
          val columns = index.columns
          val columnNames = columns.map(_.name)

          def boundsLoop(columnNames: List[String]): Option[(SearchBounds, ConstraintSet)] = columnNames match {
            case name :: tail =>
              allConstraints.get(name).flatMap { constraints =>
                val constraintsWithExps = constraints.map {
                  case (op, values) =>
                    val valuesAndExps = values.flatMap { value =>
                      // failures just mean out-of-scope values are used and so we can't use it in a scan
                      Try {
                        (value, generateExpr(value, inputs))
                      }.toOption
                    }
                    (op, valuesAndExps)
                }.filter(_._2.nonEmpty)

                if (constraintsWithExps.nonEmpty) {
                  val attr = ResolvedTableAttribute(table, scanId, name)
                  constraintsWithExps.get(Eq) match {
                    case Some(eqConstraints) =>
                      // TODO as above, this is safe, but we should check that all values in fixed constraints are equal and that they satisfy other bounds
                      val (fixedValue, fixedValueExp) = eqConstraints.head
                      val newConstraint = BinOpExpr(Eq, attr, fixedValue)
                      val (foundBounds, foundConstraints) =
                        boundsLoop(tail).getOrElse((SearchBounds.empty, ConstraintSet.empty))

                      Some((foundBounds.addFixedValue(fixedValueExp), foundConstraints.addConstraints(newConstraint)))
                    case None =>
                      def bound(isLower: Boolean): (Option[Bound], ConstraintSet) = {
                        def bound1(isInclusive: Boolean) = {
                          val op = (isLower, isInclusive) match {
                            case (true, true) => GreaterEq
                            case (true, false) => Greater
                            case (false, true) => LessEq
                            case (false, false) => Less
                          }

                          constraintsWithExps.get(op).map { valuesAndExps =>
                            val newConstraints = valuesAndExps.map {
                              case (sqlExpr, _) => BinOpExpr(op, attr, sqlExpr)
                            }
                            val boundExp = valuesAndExps.map(_._2).reduce {
                              (p1, p2) =>
                                ordOp(p1, p2) {
                                  (ord, elem) => if (isLower) OrderingMax(ord)(elem) else OrderingMin(ord)(elem)
                                }
                            }
                            (Bound(boundExp, isInclusive), newConstraints)
                          }
                        }

                        val inclusiveBound = bound1(true)
                        val exclusiveBound = bound1(false)
                        // TODO This is safe, but we can get a better bound if
                        // both are defined, using staged IF and comparison
                        exclusiveBound.orElse(inclusiveBound) match {
                          case Some((bound, sqlConstraints)) =>
                            val constraintSet = sqlConstraints.foldLeft(ConstraintSet.empty)(_.addConstraints(_))
                            (Some(bound), constraintSet)
                          case None =>
                            (None, ConstraintSet.empty)
                        }
                      }

                      val lowerBoundWithConstraints = bound(true)
                      val upperBoundWithConstraints = bound(false)

                      val lowerBound = lowerBoundWithConstraints._1
                      val upperBound = upperBoundWithConstraints._1
                      val allConstraints = lowerBoundWithConstraints._2.union(upperBoundWithConstraints._2)
                      Some(SearchBounds.range(lowerBound, upperBound), allConstraints)
                  }
                } else
                  None
              }
            case Nil => None
          }

          def hasCommonPrefix[A, B](list1: List[A], list2: List[B])(implicit ev: A =:= B) =
            list1.nonEmpty && list2.nonEmpty && list1.head == list2.head

          val goodForOrder: Option[SortDirection] = {
            if (orderColumns.nonEmpty) {
              List(Ascending, Descending).find { direction =>
                val indexColumnsForDirection =
                  columns.map(c => (c.name, c.direction.inverseIfDescending(direction), NullsOrderingUnspecified: NullsOrdering))
                hasCommonPrefix(indexColumnsForDirection, orderColumns)
              }
            } else None
          }
          val goodForGroup = groupColumns.nonEmpty && hasCommonPrefix(columnNames, groupColumns)

          val optBounds = boundsLoop(columnNames)
          if (optBounds.isDefined || goodForOrder.isDefined || goodForGroup) {
            val direction = goodForOrder.getOrElse(Ascending)
            val scannable = IndexScannable(table, index, scanId, direction, fakeDep, inputs.kernelInput)(eRow)
            val (relation, constraints) = optBounds match {
              case Some((bounds, constraints0)) =>
                (scannable.search(bounds), constraints0)
              case None =>
                (scannable.fullScan(), ConstraintSet.empty)
            }
            val indexScanOrdering =
              for (IndexedColumn(name, _, colDirection) <- index.columns)
                yield SortSpec(ResolvedTableAttribute(table, scanId, name), colDirection.inverseIfDescending(direction), NullsOrderingUnspecified)
            val plan = Plan(relation, constraints, indexScanOrdering)
            Some(plan)
          } else
            None
        }

        val relation =
          TableScannable(table, scanId, Ascending: SortDirection, fakeDep, inputs.kernelInput)(eRow).fullScan()

        val tablePlan = Plan(relation, ConstraintSet.empty, tableScanOrdering(table, scanId))

        tablePlan +: indexPlans
    }
  }

  // TODO this depends on the database, make abstract
  // This implementation is for SQLite (and MySQL?)
  def tableScanOrdering(table: Table, scanId: Int): SqlOrdering = {
    rowidColumn(table) match {
      case None =>
        Nil
      case Some(name) =>
        List(SortSpec(ResolvedTableAttribute(table, scanId, name), Ascending, NullsOrderingUnspecified))
    }
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
            case (outerPlans: Plans[a] @unchecked, innerPlan: Plan[b] @unchecked) =>
              def nodesForErrorGraph = (outerPlans :+ innerPlan).map(_.node)

              val outerRowElem = eRow(outerPlans)
              val innerRowElem = innerPlan.elem.eRow

              val makeHashJoin: Plan[a] => Plan[_] = {
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
                                !!!(s"$l and $r must be columns on opposing join sides", nodesForErrorGraph: _*)
                            case InnerScopeName =>
                              if (bindingR.scope == OuterScopeName)
                                List((r, l))
                              else
                                !!!(s"$l and $r must be columns on opposing join sides", nodesForErrorGraph: _*)
                            case _ =>
                              !!!(s"$l seems not to be a column of $outer or $inner: binding $bindingL", nodesForErrorGraph: _*)
                          }
                        case _ =>
                          !!!(s"Unsupported join condition: $cond", nodesForErrorGraph: _*)
                      }

                      columns(condition).unzip
                    }
                  }

                def keyFun[A](elem: Elem[A], op: Operator, columns: Seq[ResolvedAttribute]) = inferredFun(elem) { x =>
                  withContext(op) {
                    def column(column: ResolvedAttribute) =
                      inputs.copy(scopes = Map(currentScopeName -> x)).resolveColumn(column)

                    columns match {
                      case Seq() =>
                        !!!(s"Join using empty column list", nodesForErrorGraph: _*)
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
                    (nodesForErrorGraph :+ outerKeyFun :+ innerKeyFun): _*)
                }

                def hashJoinPlans[A, B, K](left: Plan[A], right: Plan[B], leftKeyFun: Rep[A => K], rightKeyFun: Rep[B => K]) =
                  Plan(
                    left.node.hashJoin(right.node, leftKeyFun, rightKeyFun, leftIsOuter),
                    left.constraints.union(right.constraints),
                    left.ordering)

                if (leftIsOuter)
                  hashJoinPlans(_, innerPlan, outerKeyFun, innerKeyFun)
                else
                  hashJoinPlans(innerPlan, _, innerKeyFun, outerKeyFun)
              }

              var constraints1: ConstraintSet = ConstraintSet.empty
              var ordering1: SqlOrdering = Nil

              val flatMapArg = withContext(outer) {
                inferredFun(outerRowElem) { x =>
                  val condition1 = filter match {
                    case None => condition
                    case Some(pred) => BinOpExpr(SqlAST.And, condition, pred)
                  }
                  val inner1 = resolver.pushdownFilter(inner, condition1)

                  val inner1Plan = bestPlan(inner1, innerInputs + (currentScopeName -> x)).asInstanceOf[Plan[b]]
                  constraints1 = inner1Plan.constraints
                  ordering1 = inner1Plan.ordering

                  val g = inferredFun(innerRowElem) { y =>
                    if (leftIsOuter) (x, y) else (y, x)
                  }

                  inner1Plan.node.map(g)
                }
              }

              outerPlans.flatMap { outerPlan =>
                val hashJoinUnfiltered = makeHashJoin(outerPlan)
                val hashJoin = filter match {
                  case None => hashJoinUnfiltered
                  case Some(pred) =>
                    generateFilter(hashJoinUnfiltered, join, pred, inputs)
                }

                val nestedLoopJoin = Plan(
                  outerPlan.node.flatMap(flatMapArg),
                  outerPlan.constraints.union(constraints1),
                  outerPlan.ordering ++ ordering1
                )
                Iterator[Plan[_]](hashJoin, nestedLoopJoin)
              }
          }
        }

        joinsForOrder(leftIsOuter = true) ++ joinsForOrder(leftIsOuter = false)
      case otherSpec =>
        !!!(s"Join spec $otherSpec after resolver")
    }
  }

  def generateAggregate[Row](aggregates: List[AggregateExpr], groupedBy: List[Expression], pExp: Plan[Row], inputs: ExprInputs) = {
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
      !!!(s"Distinct aggregates are not supported yet", pExp.node)
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

          // Do we need to filter out constraints on aggregated out columns?

          // TODO Use information that input is already fully or partially sorted by columns we aggregate on
          // (https://github.com/scalan/scalan-sql/issues/7)
          Plan(pExp.node.mapReduce(mapKey, newValue, reduce), pExp.constraints, Nil)
        } else {
          Plan(pExp.node.reduce(reduce, newValue), ConstraintSet.empty, Nil)
        }
    }
  }

  def generateEquality[A](table: Operator, fields: List[Expression], inputs: ExprInputs)(implicit eRow: Elem[A]) = {
    withContext(table) {
      fun[(A, A), Boolean] { x =>
        fields.foldRight(toRep(true)) {
          case (expr, acc) =>
            val lhs = generateExpr(expr, inputs + (currentScopeName -> x._1))
            val rhs = generateExpr(expr, inputs + (currentScopeName -> x._2))
            (lhs === rhs) && acc
        }
      }
    }
  }

  def generateComparator[A](table: Operator, order: SqlOrdering, inputs: ExprInputs)(implicit eRow: Elem[A]) = {
    withContext(table) {
      fun[(A, A), Boolean] { x =>
        // TODO NullsOrdering currently ignored
        order.foldRight(toRep(false)) {
          case (SortSpec(expr, direction, _), acc) =>
            val lhs = generateExpr(expr, inputs + (currentScopeName -> x._1))
            val rhs = generateExpr(expr, inputs + (currentScopeName -> x._2))
            val op = Less.inverseIfDescending(direction)
            comparisonOp(op, lhs, rhs) || ((lhs === rhs) && acc)
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

  case class ExprInputs(kernelInput: Exp[KernelInput], scanElems: Map[Scan, StructElem[_]], scopes: Map[String, Exp[_]], columnUseInfo: ColumnUseInfo) {
    def addConstraints(predicate: Expression) =
      copy(columnUseInfo = columnUseInfo.addConstraints(predicate))

    def orderByInfo(sortSpecs: SqlOrdering) = {
      def takeWhileHasUnderlyingTableColumn(sortSpecs: SqlOrdering): SqlOrdering = sortSpecs match {
        case spec :: tail =>
          val tail1 = takeWhileHasUnderlyingTableColumn(tail)
          underlyingTableColumn(spec.expr) match {
            case Some(expr1) =>
              spec.copy(expr = expr1) :: tail1
            case None => Nil
          }
        case Nil => Nil
      }

      val newOrderBy = takeWhileHasUnderlyingTableColumn(sortSpecs)
      copy(columnUseInfo = columnUseInfo.copy(orderBy = newOrderBy))
    }

    def groupByInfo(expressions: List[Expression]) = {
      val tableAttributes = expressions.map(underlyingTableColumn).takeWhile(_.isDefined).map(_.get)
      copy(columnUseInfo = columnUseInfo.copy(groupBy = tableAttributes))
    }

    def withoutOrderAndGroupInfo =
      copy(columnUseInfo = columnUseInfo.copy(orderBy = Nil, groupBy = Nil))

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
    def apply(kernelInput: Exp[KernelInput], scanElems: Map[Scan, StructElem[_]], scopes: (String, Exp[_])*): ExprInputs =
      new ExprInputs(kernelInput, scanElems, scopes.toMap, ColumnUseInfo())
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
        case op: ComparisonOp =>
          comparisonOp(op, lExp, rExp)
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
        case qExp: Plan[a] @unchecked =>
          !qExp.node.isEmpty
      }
    case LikeExpr(l, r, escape) =>
      patternMatch(l, r, escape, inputs)
    case l @ Literal(v, t) =>
      val elem = sqlTypeToElem(t)
      // non-null literals in queries are replaced by parameters, if
      // turnLiteralsIntoParameters was passed to sqlQueryExp. Only
      // in this case l.index will be Some(i) (see SqlGrammar#parseSelect).
      val currentLambdaArg = this.currentLambdaArg(inputs)
      l.index match {
        case Some(i) =>
          Parameter(i, currentLambdaArg, v)(elem)
        case None =>
          toRep(v)(elem.asElem[Any])
      }
    case p: SqlAST.Parameter =>
      val currentLambdaArg = this.currentLambdaArg(inputs)
      val i = p.index.getOrElse { !!!("Parameter doesn't have an index") }
      Parameter(i, currentLambdaArg, null)(AnyElement)
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
      // should we do anything with constraints or ordering?
      bestPlan(s, inputs).node
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
      // TODO can extra constraints from query be used when generating expr? Or vice versa?
      val exprExp = generateExpr(expr, inputs)
      bestPlan(query, inputs) match {
        case queryExp: Plan[a] @unchecked =>
          val f = inferredFun(queryExp.elem.eRow) { _.asRep[Any] === exprExp }
          !queryExp.node.filter(f).isEmpty
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
}
