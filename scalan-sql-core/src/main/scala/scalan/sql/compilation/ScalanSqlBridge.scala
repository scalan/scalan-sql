package scalan.sql.compilation

import java.sql.Date

import scala.annotation.tailrec
import scala.runtime.IntRef
import scalan.sql.parser.{SqlAST, SqlParser, SqlResolver}
import SqlAST._
import scalan.sql.ScalanSqlExp

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
    val tables = allTables(parsedSql).toSeq.distinct

    val inputRowElems = tables.map {
      case (name, table) =>
        // TODO include only required columns
        val eRow = tableElems.getOrElse(table.name, !!!(s"Table $name not found"))
        (name, eRow)
    }

    val eInput = structElement(inputRowElems.map {
      case (name, eRow) => (name, funcElement(IntElement, relationElement(eRow)))
    } :+ fakeDepField)
    val resultRelationFun = inferredFun(eInput) { x =>
      val tableScopes = tables.map {
        case (name, table) => (name, field(x, name))
      }
      val scopes = tableScopes :+ (currentScopeName -> x)
      val inputs = ExprInputs(scopes: _*)
      generateOperator(parsedSql, inputs)
    }
    // could return a struct instead of adding metadate, function is easier to pass to compiler for now
    resultRelationFun.setMetadata(QueryTextKey)(query)

    resultRelationFun
  }

  object ScanOrScanAlias {
    def unapply(op: Operator) = op match {
      case Scan(tableName, _) => Some(tableName -> resolver.table(tableName))
      case TableAlias(Scan(tableName, _), name) => Some(s"$tableName as $name" -> resolver.table(tableName))
      case _ => None
    }
  }

  def allTables(op: Any): Iterator[(String, Table)] = op match {
    case ScanOrScanAlias(relationName, table) =>
      Iterator(relationName -> table)
    case p: Product =>
      p.productIterator.flatMap(allTables)
    case _ => Iterator.empty
  }

  def generateJoin(outer: Operator, inner: Operator, joinType: JoinType, joinSpec: JoinSpec, inputs: ExprInputs) = {
    (generateOperator(outer, inputs), generateOperator(inner, inputs)) match {
      case (outerExp: RRelation[a] @unchecked, innerExp: RRelation[b] @unchecked) =>
        val outerRowElem = outerExp.elem.eRow
        val innerRowElem = innerExp.elem.eRow

        val (outerJoinColumns, innerJoinColumns) = joinSpec match {
          case Natural =>
            val allOuterColumnNames = outerRowElem.asInstanceOf[StructElem[_]].fieldNames
            val allInnerColumnNames = innerRowElem.asInstanceOf[StructElem[_]].fieldNames
            val commonColumns = allOuterColumnNames.intersect(allInnerColumnNames).map(UnresolvedAttribute(None, _))
            (commonColumns, commonColumns)
          case Using(columnNames) =>
            val columns = columnNames.map(UnresolvedAttribute(None, _))
            (columns, columns)
          case On(condition) =>
            withContext(outer) {
              val OuterScopeName = currentScopeName
              withContext(inner) {
                val InnerScopeName = currentScopeName

                def columns(cond: Expression): List[(UnresolvedAttribute, UnresolvedAttribute)] = cond match {
                  case BinOpExpr(SqlAST.And, l, r) =>
                    columns(l) ++ columns(r)
                  case BinOpExpr(Eq, l: UnresolvedAttribute, r: UnresolvedAttribute) =>
                    val bindingL = resolver.lookup(l)
                    val bindingR = resolver.lookup(r)
                    bindingL.scope match {
                      case OuterScopeName =>
                        if (bindingR.scope == InnerScopeName)
                          List((l, r))
                        else
                          !!!(s"$l and $r must be columns on opposing join sides", outerExp, innerExp)
                      case InnerScopeName =>
                        if (bindingR.scope == OuterScopeName)
                          List((r, l))
                        else
                          !!!(s"$l and $r must be columns on opposing join sides", outerExp, innerExp)
                      case _ =>
                        !!!(s"$l seems not to be a column of $outer or $inner: binding $bindingL", outerExp, innerExp)
                    }
                  case _ =>
                    !!!(s"Unsupported join condition: $cond", outerExp, innerExp)
                }

                columns(condition).unzip
              }
            }
        }

        def keyFun[A](elem: Elem[A], op: Operator, columns: Seq[UnresolvedAttribute]) = inferredFun(elem) { x =>
          withContext(op) {
            def column(column: UnresolvedAttribute) =
              ExprInputs(currentScopeName -> x).resolveColumn(column)

            columns match {
              case Seq() =>
                !!!(s"Join using empty column list", outerExp, innerExp)
              case Seq(col) =>
                column(col)
              case _ =>
                val fields = columns.map(column _)
                tupleStruct(fields: _*)
            }
          }
        }

        val outerKeyFun = keyFun(outerRowElem, outer, outerJoinColumns)
        val innerKeyFun = keyFun(innerRowElem, inner, innerJoinColumns)
        if (outerKeyFun.elem.eRange != innerKeyFun.elem.eRange) {
          !!!(s"Different key types for two join sides: ${outerKeyFun.elem.eRange} and ${innerKeyFun.elem.eRange}",
            outerExp, innerExp, outerKeyFun, innerKeyFun)
        }
        if (joinType != Inner) {
          !!!("Non-inner joins are not supported yet", outerExp, innerExp)
        }
        outerExp.join(innerExp, outerKeyFun, innerKeyFun)
    }
  }

  def generateOperator(op: Operator, inputs: ExprInputs): Exp[Relation[_]] = ((op match {
    case Join(outer, inner, joinType, joinSpec) =>
      generateJoin(outer, inner, joinType, joinSpec, inputs)
    case ScanOrScanAlias(relationName, table) =>
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

      inputs(relationName).asInstanceOf[RFunc[Int, Relation[_]]](fakeDep)
    case OrderBy(p, by) =>
      generateOperator(p, inputs) match {
        case pExp: RRelation[a] @unchecked =>
          val eRow = pExp.elem.eRow
          val comparator = generateComparator(p, by, inputs)(eRow)
          pExp.sort(comparator)
      }
    case GroupBy(agg, by) =>
      agg match {
        case Project(p, columns) =>
          withContext(p) {
            generateOperator(p, inputs) match {
              case pExp: RRelation[a] @unchecked =>
                generateAggregate(columns, by, pExp, inputs)
            }
          }
        case _ => throw SqlException("Unsupported group-by clause")
      }

    case Filter(p, predicate) =>
      val (joins, conjuncts) = resolver.optimize(p, predicate)
      generateOperator(joins, inputs) match {
        case joinsExp: RRelation[a] @unchecked =>
          conjuncts match {
            case Literal(_, _) => // Is this right?
              joinsExp
            case _ =>
              val eRow = joinsExp.elem.eRow
              joinsExp.filter(generateLambdaExpr(p, conjuncts, inputs)(eRow).asRep[a => Boolean])
          }
      }
    case Project(p, columns) =>
      withContext(p) {
        generateOperator(p, inputs) match {
          case pExp: RRelation[a] @unchecked =>
            columns.find(resolver.isAggregate) match {
              case None =>
                val structLambda = generateStructLambdaExpr(p, columns, inputs)(pExp.elem.eRow).asRep[a => Any]
                pExp.map(structLambda)
              case Some(aggColumn) =>
                columns.find(c => !resolver.isAggregate(c)) match {
                  case None =>
                    generateAggregate(columns, Nil, pExp, inputs)
                  case Some(nonAggColumn) =>
                    !!!(s"Query contains both aggregate $aggColumn and non-aggregate $nonAggColumn", pExp)
                }
            }
        }
      }
    case TableAlias(operator, alias) =>
      assert(!operator.isInstanceOf[Scan], s"$op should be handled in ScanOrScanAlias case")
      generateOperator(operator, inputs)
    case SubSelect(p) =>
      generateOperator(p, inputs)
    case _ =>
      ???(s"Failed to construct Scalan graph from SQL AST $op")
  }): Exp[Relation[_]]).setMetadata(SqlOperatorKey)(op)

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

  def generateAggregate[Row](columns: List[ProjectionColumn], groupedBy: List[Expression], pExp: Exp[Relation[Row]], inputs: ExprInputs) = {
    def simplifyAgg(x: Expression): Expression = x match {
      case AggregateExpr(Count, false, _) =>
        CountAllExpr
      case AggregateExpr(Avg, distinct, value) =>
        val normalizedValue = simplifyAgg(value)
        val sum = AggregateExpr(Sum, distinct, normalizedValue)
        val count = if (distinct)
          AggregateExpr(Count, distinct, normalizedValue)
        else
          CountAllExpr
        BinOpExpr(Divide, CastExpr(sum, DoubleType), CastExpr(count, DoubleType))
      case x: Literal => x
      // ugly way to test for an `object`
      case x if x.getClass.getSimpleName.endsWith("$") => x
      case p: Product =>
        val constructor = x.getClass.getConstructors.apply(0)
        val members = p.productIterator.map {
          case e: Expression => simplifyAgg(e)
          case x => x.asInstanceOf[AnyRef]
        }.toArray
        constructor.newInstance(members: _*).asInstanceOf[Expression]
    }

    def extractAggregateExprs(expr: Expression): List[AggregateExpr] = expr match {
      case agg: AggregateExpr =>
        List(agg)
      case BinOpExpr(_, l, r) =>
        extractAggregateExprs(l) ++ extractAggregateExprs(r)
      case LikeExpr(l, r, escape) =>
        extractAggregateExprs(l) ++ extractAggregateExprs(r) ++
          (escape match {
            case None => Nil
            case Some(escape) => extractAggregateExprs(escape)
          })
      case NegExpr(opd) =>
        extractAggregateExprs(opd)
      case NotExpr(opd) =>
        extractAggregateExprs(opd)
      case Literal(v, t) =>
        Nil
      case CastExpr(exp, typ) =>
        extractAggregateExprs(exp)
      case ref: UnresolvedAttribute =>
        Nil
      case SubstrExpr(str, from, len) =>
        extractAggregateExprs(str) ++ extractAggregateExprs(from) ++ extractAggregateExprs(len)
      case CaseWhenExpr(list) =>
        list.flatMap(extractAggregateExprs)
      case InListExpr(sel, list) =>
        extractAggregateExprs(sel) ++ list.flatMap(extractAggregateExprs)
      // case SelectExpr(s) =>
      // case InExpr(sel, query) =>
      case FuncExpr(name, args) =>
        args.flatMap(extractAggregateExprs)
      case _ =>
        !!!(s"Don't know how to extract aggregate parts from a ${expr.getClass.getSimpleName}")
    }

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

    def aggFinalResult(columns: List[ProjectionColumn], groupedBy: ExprList, aggregates: List[AggregateExpr], key: Exp[Struct], value: Exp[Struct], trackSyms: Exp[_]*): Exp[Struct] = {
      val unnamedCounter = new IntRef(0)
      val outputFields = columns.map { column =>
        val name = this.name(column, unnamedCounter)
        // Each column is assumed to be contained in `by` or in `aggregates`.
        // Possibility that it's uniquely determined by `by` isn't covered currently.
        val fieldValue = groupedBy.indexWhere(resolver.matchExpr(_, column.alias, column.expr)) match {
          case -1 =>
            try {
              generateExpr(column.expr, ExprInputs(inputs.scopes, aggregates, value))
            } catch {
              case AggregateNotFound(agg, list) =>
                !!!(s"Aggregate $agg is part of $column but not found among aggregate values", trackSyms: _*)
            }
          case n =>
            field(key, n)
        }
        (name, fieldValue)
      }

      struct(outputFields: _*)
    }

    implicit val eRow = pExp.elem.eRow

    val normalizedColumns = columns.map { col =>
      val normalized = simplifyAgg(col.expr)
      col.copy(expr = normalized)
    }

    val aggregates = normalizedColumns.flatMap { col =>
      extractAggregateExprs(col.expr)
    }.distinct

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
          val eK = mapKey.elem.eRange

          val eKV = keyValElem(eK, eV)

          val reduced = pExp.mapReduce(mapKey, newValue, reduce)

          val finalProjection = inferredFun(eKV) { in =>
            val key = field(in, keyFld).asRep[Struct]
            val value = field(in, valFld).asRep[Struct]
            aggFinalResult(normalizedColumns, groupedBy, aggregates, key, value)
          }

          reduced.map(finalProjection)
        } else {
          val reduced = pExp.reduce(reduce, newValue)

          val finalProjection = inferredFun(eV) { in =>
            aggFinalResult(normalizedColumns, Nil, aggregates, null, in.asRep[Struct])
          }

          reduced.map(finalProjection)
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
      column.expr match {
        case c: UnresolvedAttribute =>
          c.name
        case _ =>
          unnamedCounter.elem += 1
          s"_${unnamedCounter.elem}"
      }
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

  private def ordOp[A, B](l: Exp[A], r: Exp[B])(f: (Ordering[A], Elem[A]) => BinOp[A, Boolean]) = {
    val LRHS(l1, r1, elem) = widen(l, r)
    val ord = getOrdering(elem)
    f(ord.asInstanceOf[Ordering[A]], elem.asElem[A])(l1.asRep[A], r1.asRep[A])
  }

  case class AggregateNotFound(agg: AggregateExpr, list: List[AggregateExpr]) extends Exception

  case class ExprInputs(scopes: Map[String, Exp[_]], aggregates: List[AggregateExpr], value: Exp[Struct]) {
    def resolveAgg(agg: AggregateExpr) =
      aggregates.indexOf(agg) match {
        case -1 =>
          throw new AggregateNotFound(agg, aggregates)
        case n =>
          field(value, n)
      }

    // TODO inline
    def resolveColumn(c: UnresolvedAttribute) = {
      val binding = resolver.lookup(c)
      scopes.get(binding.scope) match {
        case None =>
          !!!(s"Failed to resolve $c. Binding is $binding.")
        case Some(x) =>
          def _1(y: Exp[_]) =
            y.elem match {
              case pe: PairElem[a, b] =>
                y.asRep[(a, b)]._1
              case se: StructElem[_] =>
                field(y.asRep[Struct], 0)
            }
          def _2(y: Exp[_]) =
            y.elem match {
              case pe: PairElem[a, b] =>
                y.asRep[(a, b)]._2
              case se: StructElem[_] =>
                field(y.asRep[Struct], 1)
            }

          binding.path.foldLeft[Exp[_]](x) {
            case (y, "head") =>
              _1(y)
            case (y, "tail") =>
              _2(y)
            case (y, fieldName) =>
              // TODO fix bindings in SqlCompiler to avoid the need for this code
              y.elem match {
                case pe: PairElem[a, b] =>
                  fieldName match {
                    case "head" =>
                      _1(y)
                    case "tail" =>
                      _2(y)
                    case _ =>
                      @tailrec
                      def tupleMemberByIndex(z: Exp[_], index: Int): Exp[_] = {
                        index match {
                          case 1 =>
                            _1(z)
                          case 2 if !z.elem.isInstanceOf[PairElem[_, _]] =>
                            _2(z)
                          case _ =>
                            tupleMemberByIndex(_2(z), index - 1)
                        }
                      }

                      try {
                        val index = fieldName.stripPrefix("_").toInt
                        tupleMemberByIndex(y, index)
                      } catch {
                        case e: NumberFormatException =>
                          !!!(s"Tried to get field named $fieldName from a tuple $y", y)
                      }
                  }
                case se: StructElem[_] =>
                  val fieldName1 = try {
                    val index = fieldName.stripPrefix("_").toInt
                    se.fieldNames(index - 1)
                  } catch {
                    case e: NumberFormatException =>
                      fieldName
                  }
                  field(y.asRep[Struct], fieldName1)
              }
          }
      }
    }

    def +(pair: (String, Exp[_])) = copy(scopes = scopes + pair)

    def apply(name: String) = scopes(name)
  }

  object ExprInputs {
    def apply(scopes: (String, Exp[_])*): ExprInputs = new ExprInputs(scopes.toMap, Nil, null)
    def apply(aggregates: List[AggregateExpr], value: Exp[Struct]) = new ExprInputs(Map.empty, aggregates, value)
  }

def generateExpr(expr: Expression, inputs: ExprInputs): Exp[_] = ((expr match {
    case agg: AggregateExpr =>
      inputs.resolveAgg(agg)
    case c: UnresolvedAttribute =>
      inputs.resolveColumn(c)
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
      generateOperator(q, inputs) match {
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
      generateOperator(s, inputs)
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
      generateOperator(query, inputs) match {
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
