package scalan.sql.parser

import scala.annotation.tailrec
import scalan.sql.parser.SqlAST._
import scalan.util.ReflectionUtil

class SqlResolver(val schema: Schema) {
  def table(name: String) = schema.table(name)
  def indices(tableName: String) = schema.indices(tableName)

  case class Scope(var ctx: Context, outer: Option[Scope], nesting: Int, name: String) {
    def resolve(unresolved: UnresolvedAttribute): ResolvedAttribute =
      ctx.resolveColumn(unresolved).getOrElse {
        outer match {
          case Some(s) =>
            s.resolve(unresolved)
          case None =>
            throw SqlException(s"Failed to resolve column $unresolved")
        }
      }

    def lookup(col: ResolvedAttribute): Binding = {
      ctx.lookup(col) match {
        case Some(b) => b
        case None => {
          outer match {
            case Some(s: Scope) =>
              s.lookup(col)
            case _ =>
              throw SqlException(s"Failed to lookup column $col")
          }
        }
      }
    }
  }

  var currScope: Scope = Scope(GlobalContext, None, 0, "scalan")

  def withContext[A](op: Operator)(block: => A) = {
    currScope = Scope(currScope.ctx, Some(currScope), currScope.nesting + 1, if (currScope.nesting == 0) "r" else "r" + currScope.nesting.toString)
    currScope.ctx = buildContext(op)
    val result = block
    currScope = currScope.outer.get
    result
  }

  case class ResolutionInputs(requiredAttributes: Set[UnresolvedAttribute]) {
    def unresolvedAttributes(op: Any): Iterator[UnresolvedAttribute] = op match {
      case attr: UnresolvedAttribute => Iterator(attr)
      case seq: Seq[_] => seq.iterator.flatMap(unresolvedAttributes)
      case p: Product => p.productIterator.flatMap(unresolvedAttributes)
      case _ => Iterator.empty
    }

    def add(expressions: List[Expression]): ResolutionInputs =
      ResolutionInputs(requiredAttributes ++ unresolvedAttributes(expressions))
  }

  case class ResolutionOutput(op: Operator, optProjection: Option[List[ProjectionColumn]]) {
    def map(f: Operator => Operator) = ResolutionOutput(f(op), optProjection)
    def project() =
      optProjection.fold(op)(projection => Project(op, projection))
    def projectIfEnough(inputs: ResolutionInputs) = optProjection match {
      case None =>
        this
      case Some(finalProjection) =>
        val projected = project()
        val ctx = buildContext(projected)
        if (inputs.requiredAttributes.forall(ctx.resolveColumn(_).isDefined))
          ResolutionOutput(projected, None)
        else
          this
    }
  }

  def resolveAggregates(parent: Operator, groupedBy: ExprList, columns: List[SelectListElement], inputs: ResolutionInputs): ResolutionOutput = {
    def simplifyAgg(x: Expression): Expression = x match {
      case AggregateExpr(Count, false, _) =>
        CountAllExpr
      case AggregateExpr(op, distinct, value) =>
        val normalizedValue = simplifyAgg(resolveExpr(value))
        op match {
          case Avg =>
            val sum = AggregateExpr(Sum, distinct, normalizedValue)
            val count = if (distinct)
              AggregateExpr(Count, distinct, normalizedValue)
            else
              CountAllExpr
            BinOpExpr(Divide, CastExpr(sum, DoubleType), CastExpr(count, DoubleType))
          case _ =>
            AggregateExpr(op, distinct, normalizedValue)
        }
      case x: Literal => x
      case x if ReflectionUtil.isSingleton(x) => x
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
        extractAggregateExprs(l) ++ extractAggregateExprs(r) ++ escape.toList.flatMap(extractAggregateExprs)
      case NegExpr(opd) =>
        extractAggregateExprs(opd)
      case NotExpr(opd) =>
        extractAggregateExprs(opd)
      case _: Literal | _: Parameter =>
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
        throw new SqlException(s"Don't know how to extract aggregate parts from a ${expr.getClass.getSimpleName}")
    }

    columns.find {
      case ProjectionColumn(expr, _) =>
        !(containsAggregates(expr) || groupedBy.contains(expr))
      case UnresolvedStar(_) =>
        true
    } match {
      case Some(projectionColumn) =>
        throw new SqlException(s"Non-aggregate column $projectionColumn in an aggregate query")
      case None =>
        val normalizedColumns = columns.map {
          case col: ProjectionColumn => col.mapExpr(simplifyAgg)
          case _ => throw new SqlException("Impossible")
        }

        val aggregates = normalizedColumns.flatMap { col =>
          extractAggregateExprs(col.expr)
        }.distinct

        val aggregate = Aggregate(parent, groupedBy.map(resolveExpr), aggregates)

        withContext(aggregate) {
          resolveNonAggregateProjectionWithResolvedParent(aggregate, normalizedColumns, inputs)
        }
    }
  }

  def resolveOperator(op: Operator) = {
    resolveOperator0(op, ResolutionInputs(Set.empty)).project()
  }

  private def resolveOperator0(op: Operator, inputs: ResolutionInputs): ResolutionOutput = op match {
    case Scan(tableName, _) =>
      ResolutionOutput(op, None)
    case Distinct(op) =>
      resolveOperator0(op, inputs).map(Distinct.apply)
    case Union(left, right) =>
      resolveSetOp(left, right, inputs)(Union.apply)
    case Except(left, right) =>
      resolveSetOp(left, right, inputs)(Except.apply)
    case Intersect(left, right) =>
      resolveSetOp(left, right, inputs)(Intersect.apply)
    case TableAlias(table, alias) =>
      // TODO alias should be removed during resolution, but we need to make sure AliasContext is produced
      // For now aliases are just ignored in ScalanSqlBridge
      resolveOperator0(table, inputs).map(TableAlias(_, alias))
    case Filter(parent, predicate) =>
      resolveOperatorAddingExpression(parent, inputs, List(predicate)) {parent1 =>
        val predicate1 = withContext(parent1) {
          resolveExpr(predicate)
        }
        pushdownFilter(parent1, predicate1)
      }
    case Project(parent, columns) =>
      val ResolutionOutput(parent1, optProj1) = resolveOperator0(parent, inputs)
      withContext(parent1) {
        if (columns.exists(containsAggregates)) {
          val out1 = resolveAggregates(parent1, Nil, columns, inputs)
          // TODO what should be done if this condition fails? Can it fail?
          assert(out1.optProjection.isEmpty || out1.optProjection == optProj1)
          ResolutionOutput(out1.op, optProj1)
        }
        else
          resolveNonAggregateProjectionWithResolvedParent(parent1, columns, inputs)
      }
    case GroupBy(parent, groupedBy) =>
      val (parent1, columns) = parent match {
        case Project(parent1, columns) =>
          (parent1, columns)
        case Filter(Project(parent1, columns), predicate) =>
          (Filter(parent1, predicate), columns)
        case _ =>
          throw new SqlException(s"Unexpected parent for a GroupBy:\n$parent")
      }
      resolveOperatorAddingExpression0(parent1, inputs, groupedBy) { (parent2, inputs1) =>
        withContext(parent2) {
          resolveAggregates(parent2, groupedBy, columns, inputs1)
        }
      }
    case OrderBy(parent, columns) =>
      resolveOperatorAddingExpression(parent, inputs, columns.map(_.expr)) { parent1 =>
        withContext(parent1) {
          OrderBy(parent1, columns.map(s => s.copy(expr = resolveExpr(s.expr))))
        }
      }
    case Limit(parent, limit) =>
      resolveOperator0(parent, inputs).map { parent1 =>
        withContext(parent1) {
          Limit(parent1, resolveExpr(limit))
        }
      }
    case SubSelect(parent) =>
      resolveOperator0(parent, inputs)
    case Join(left, right, joinType, spec) =>
      // resolveOperator0(left, inputs).optProjection should only be non-empty if left is a subquery,
      // in which case it should be projected immediately.
      // At any rate indexes in optProjection won't be valid after join.
      val left1 = resolveOperator0(left, inputs).project()
      // Same for right
      val right1 = resolveOperator0(right, inputs).project()
      val condition1 = spec match {
        case On(condition) =>
          withContext(left1) {
            withContext(right1) {
              resolveExpr(condition)
            }
          }
        case Using(columns) =>
          def resolvedColumns() = columns.map(name => resolveExpr(UnresolvedAttribute(None, name)))
          val cLeft = withContext(left1) {
            resolvedColumns()
          }
          val cRight = withContext(right1) {
            resolvedColumns()
          }
          val equalities = (cLeft, cRight).zipped.map(BinOpExpr(Eq, _, _))
          conjunction(equalities)
        case Natural =>
          // requires implementing operatorType, won't support it yet
          throw new NotImplementedError("Natural joins not supported yet")
      }
      ResolutionOutput(Join(left1, right1, joinType, On(condition1)), None)
    case CrossJoin(outer, inner) =>
      resolveSetOp(outer, inner, inputs)(CrossJoin.apply)
    case UnionJoin(outer, inner) =>
      resolveSetOp(outer, inner, inputs)(UnionJoin.apply)
    case _ =>
      throw new NotImplementedError(s"Can't resolve operator\n$op")
  }

  def resolveNonAggregateProjectionWithResolvedParent(parent: Operator, columns: List[SelectListElement], inputs: ResolutionInputs): ResolutionOutput = {
    val columns1 = columns.flatMap {
      case ProjectionColumn(expr, alias) =>
        List(ProjectionColumn(resolveExpr(expr), alias))
      case UnresolvedStar(qualifier) =>
        currScope.ctx.resolveStar(qualifier).getOrElse {
          throw new SqlException(s"Can't resolve ${qualifier.fold("")(_ + ".")}*")
        }
    }
    // TODO debug and simplify! Move logic to ResolutionInputs?
    val res1 = Project(parent, columns1)
    val ctx1 = buildContext(res1)
    val missingColumns = inputs.requiredAttributes.collect {
      case col if ctx1.resolveColumn(col).isEmpty =>
        ProjectionColumn(col, None)
    }
    if (missingColumns.isEmpty)
      ResolutionOutput(res1, None)
    else {
      // go into recursion, but this time missingColumns should be empty!
      val out1 = resolveNonAggregateProjectionWithResolvedParent(parent, columns1 ++ missingColumns, inputs)
      assert(out1.optProjection.isEmpty)
      val columns2 = columns1.zipWithIndex.map {
        case (col, i) =>
          ProjectionColumn(ResolvedProjectedAttribute(col.expr, None, i), col.alias)
      }
      ResolutionOutput(out1.op, Some(columns2))
    }
  }

  private def resolveOperatorAddingExpression(op: Operator, inputs: ResolutionInputs, extraExpressions: List[Expression])
                                             (f: Operator => Operator): ResolutionOutput = {
    val out1 = resolveOperator0(op, inputs.add(extraExpressions))
    val out2 = out1.map(f)
    out2.projectIfEnough(inputs)
  }

  private def resolveOperatorAddingExpression0(op: Operator, inputs: ResolutionInputs, extraExpressions: List[Expression])
                                              (f: (Operator, ResolutionInputs) => ResolutionOutput): ResolutionOutput = {
    val inputs1 = inputs.add(extraExpressions)
    val out1 = resolveOperator0(op, inputs1)
    val out2 = f(out1.op, inputs1) // TODO or inputs?
    out2.projectIfEnough(inputs)
  }

  private def resolveSetOp(op1: Operator, op2: Operator, inputs: ResolutionInputs)(f: (Operator, Operator) => Operator) = {
    val ResolutionOutput(op1out, finalProjection1) = resolveOperator0(op1, inputs)
    val ResolutionOutput(op2out, finalProjection2) = resolveOperator0(op1, inputs)
    val finalProjection = (finalProjection1, finalProjection2) match {
      case (Some(p1), Some(p2)) if p1 != p2 =>
        throw new SqlException(s"Different projections required from resolving $op1 and $op2 under $inputs, don't know what to do")
      case _ => finalProjection1.orElse(finalProjection2)
    }
    ResolutionOutput(f(op1out, op2out), finalProjection)
  }


  def pushdownFilter(parent: Operator, predicate: Expression): Operator = {
    val clauses = conjunctiveClauses(predicate)

    // returns the list of clauses which remain unhandled
    def doPushdown(op: Operator, clauses: List[Expression]): (Operator, List[Expression]) = op match {
      // TODO handle other op cases
      case Join(left, right, joinType, joinSpec) =>
        val (clausesDependingOnRight, leftOnlyClauses) = clauses.partition(depends(right, _))
        val left1 = doPushdownWithNoRemainingClauses(left, leftOnlyClauses)
        val (clausesDependingOnBoth, rightOnlyClauses) = clausesDependingOnRight.partition(depends(left, _))
        val right1 = doPushdownWithNoRemainingClauses(right, rightOnlyClauses)
        (Join(left1, right1, joinType, joinSpec), clausesDependingOnBoth)
      case Filter(parent, condition) =>
        doPushdown(parent, (clauses ++ conjunctiveClauses(condition)).distinct)
      case Project(parent, columns) =>
        def pushdownIfAllNotDerived(exprs: List[Expression]): Option[List[Expression]] = exprs match {
          case Nil => Some(Nil)
          case head :: tail =>
            for {
              head1 <- pushdownIfNotDerived(head)
              tail1 <- pushdownIfAllNotDerived(tail)
            } yield head1 :: tail1
        }

        def pushdownIfNotDerived(expr: Expression): Option[Expression] = expr match {
          case _: Literal | _: Parameter | _: ResolvedTableAttribute =>
            Some(expr)
          case ResolvedProjectedAttribute(parent, _, index) =>
            parent match {
              case nonDerived: ResolvedAttribute => Some(nonDerived)
              case _ => None
            }
          case BinOpExpr(op, left, right) =>
            for {
              left1 <- pushdownIfNotDerived(left)
              right1 <- pushdownIfNotDerived(right)
            } yield BinOpExpr(op, left1, right1)
          case LikeExpr(left, right, escape) =>
            for {
              left1 <- pushdownIfNotDerived(left)
              right1 <- pushdownIfNotDerived(right)
              escape1 <- escape match {
                case None => Some(None)
                case Some(e) => pushdownIfNotDerived(e).map(Some.apply)
              }
            } yield LikeExpr(left1, right1, escape1)
          case NegExpr(opd) =>
            pushdownIfNotDerived(opd).map(NegExpr)
          case NotExpr(opd) =>
            pushdownIfNotDerived(opd).map(NotExpr)
          case CastExpr(exp, tpe) => 
            pushdownIfNotDerived(exp).map(CastExpr(_, tpe))
          case AggregateExpr(op, distinct, opd) =>
            pushdownIfNotDerived(opd).map(AggregateExpr(op, distinct, _))
          case SubstrExpr(str, from, len) =>
            for {
              str1 <- pushdownIfNotDerived(str)
              from1 <- pushdownIfNotDerived(from)
              len1 <- pushdownIfNotDerived(len)
            } yield SubstrExpr(str1, from1, len1)
          case CaseWhenExpr(list) =>
            pushdownIfAllNotDerived(list).map(CaseWhenExpr)
          case InListExpr(sel, lst) =>
            for {
              sel1 <- pushdownIfNotDerived(sel)
              lst1 <- pushdownIfAllNotDerived(lst)
            } yield InListExpr(sel1, lst1)
          case ExistsExpr(q) =>
            // TODO query also needs to be pushed down if possible (below as well)
            // pushdownIfNotDerived(q).map(ExistsExpr)
            None
          case SelectExpr(s) =>
            // pushdownIfNotDerived(s).map(SelectExpr)
            None
          case InExpr(sel, query) =>
            // pushdownIfNotDerived(sel).map(InExpr(_, query))
            None
          case FuncExpr(name, args) =>
            pushdownIfAllNotDerived(args).map(FuncExpr(name, _))
          case _ =>
            throw new SqlException(s"Missing case for doPushdown into a Project: $expr")
        }

        def partitionPushdownableClauses(clauses: List[Expression]): (List[Expression], List[Expression]) = clauses match {
          case Nil => (Nil, Nil)
          case head :: tail =>
            val (pushedDownT, nonPushdownableT) = partitionPushdownableClauses(tail)
            pushdownIfNotDerived(head) match {
              case None =>
                (pushedDownT, head :: nonPushdownableT)
              case Some(pushedDownH) =>
                (pushedDownH :: pushedDownT, nonPushdownableT)
            }
        }

        val (pushedDown, remaining) = partitionPushdownableClauses(clauses)
        val parent1 = doPushdownWithNoRemainingClauses(parent, pushedDown)
        (Project(parent1, columns), remaining)
      case _ => (op, clauses)
    }

    def doPushdownWithNoRemainingClauses(op: Operator, clauses: List[Expression]) = {
      if (clauses.isEmpty)
        op
      else {
        val (pushedDown, remainingClauses) = doPushdown(op, clauses)
        if (remainingClauses.isEmpty) {
          pushedDown
        } else {
          // TODO check if repeated filters perform better due to lack of shortcutting
          val remainingPredicate = conjunction(remainingClauses)
          Filter(pushedDown, remainingPredicate)
        }
      }
    }

    doPushdownWithNoRemainingClauses(parent, clauses)
  }

  // TODO should ResolutionInputs be passed here and resolveOperator0 be used for subqueries?
  def resolveExpr(expr: Expression): Expression = expr match {
    case SelectExpr(op) =>
      SelectExpr(resolveOperator(op))
    case BinOpExpr(op, left, right) =>
      BinOpExpr(op, resolveExpr(left), resolveExpr(right))
    case agg @ AggregateExpr(op, distinct, value) =>
      currScope.ctx.resolveAggregate(agg).getOrElse {
        throw new SqlException(s"Aggregate $agg not found")
      }
      // AggregateExpr(op, distinct, resolveExpr(value))
    case LikeExpr(left, right, escape) =>
      LikeExpr(resolveExpr(left), resolveExpr(right), escape.map(resolveExpr))
    case InListExpr(expr, list) =>
      InListExpr(resolveExpr(expr), list.map(resolveExpr))
    case InExpr(left, subquery) =>
      InExpr(resolveExpr(left), resolveOperator(subquery))
    case ExistsExpr(op) =>
      ExistsExpr(resolveOperator(op))
    case NegExpr(opd) =>
      NegExpr(resolveExpr(opd))
    case NotExpr(opd) =>
      NotExpr(resolveExpr(opd))
    case SubstrExpr(str, from, len) =>
      SubstrExpr(resolveExpr(str), resolveExpr(from), resolveExpr(len))
    case IsNullExpr(opd) =>
      IsNullExpr(resolveExpr(opd))
    case FuncExpr(name, params) =>
      FuncExpr(name, params.map(resolveExpr))
    case CastExpr(expr, to) =>
      CastExpr(resolveExpr(expr), to)
    case CaseWhenExpr(list) =>
      CaseWhenExpr(list.map(resolveExpr))
    case Literal(_, _) | NullLiteral | _: ResolvedAttribute | _: Parameter =>
      expr
    case unresolved: UnresolvedAttribute =>
      currScope.resolve(unresolved)
  }

  def lookup(col: ResolvedAttribute): Binding = currScope.lookup(col)

  def tablesInNestedSelects(e: Expression): Set[Table] = {
    e match {
      case BinOpExpr(op, l, r) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case ExistsExpr(q) => tables(q)
      case LikeExpr(l, r, _) => tablesInNestedSelects(l) ++ tablesInNestedSelects(r)
      case NegExpr(opd) => tablesInNestedSelects(opd)
      case NotExpr(opd) => tablesInNestedSelects(opd)
      case CastExpr(exp, typ) => tablesInNestedSelects(exp)
      case SelectExpr(s) => tables(s)
      case InExpr(s, q) => tablesInNestedSelects(s) ++ tables(q)
      case _ => Set()
    }
  }

  def tables(op: Operator): Set[Table] = {
    op match {
      case Join(left, right, _, _) => tables(left) ++ tables(right)
      case Scan(t, _) => Set(table(t))
      case OrderBy(p, _) => tables(p)
      case Aggregate(p, _, _) => tables(p)
      case Filter(p, predicate) => tables(p) ++ tablesInNestedSelects(predicate)
      case Project(p, columns) => tables(p)
      case TableAlias(t, a) => tables(t)
      case SubSelect(p) => tables(p)
      case _ => throw new NotImplementedError(s"tables($op)")
    }
  }

  sealed trait Selector
  case class Field(name: String) extends Selector
  case class Index(i: Int) extends Selector
  case object First extends Selector
  case object Second extends Selector

  case class Binding(scope: String, path: List[Selector])

  // This is SQLite-specific implementation, override for other DBs
  /** call only after ensuring there is no column with this name! */
  protected def isImplicitRowidColumn(columnName: String, table: Table) =
    (columnName == "rowid" || columnName == "_rowid_" || columnName == "oid") && !table.withoutRowId

  def resolvedTableAttributeByName(table: Table, tableId: Int, columnName: String) = {
    table.columns.indexWhere(_.name == columnName) match {
      case -1 if !isImplicitRowidColumn(columnName, table) =>
        None
      case i =>
        Some(ResolvedTableAttribute(table, tableId, i))
    }
  }

  abstract class Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute]
    def resolveAggregate(agg: AggregateExpr): Option[ResolvedAttribute] = None
    def resolveStar(qualifier: Option[String]): Option[List[ProjectionColumn]]
    def lookup(resolved: ResolvedAttribute): Option[Binding]
    def lookup(unresolved: UnresolvedAttribute): Option[Binding] = resolveColumn(unresolved).flatMap(lookup)

    val scope = currScope
  }

  case object GlobalContext extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None
    def resolveStar(qualifier: Option[String]) = None
    def lookup(resolved: ResolvedAttribute): Option[Binding] = None
  }

  case class TableContext(table: Table, id: Int) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] =
      ifQualifierMatches(ref.table) {
        resolvedTableAttributeByName(table, id, ref.name)
      }

    def resolveStar(qualifier: Option[String]) =
      ifQualifierMatches(qualifier) {
        val projectionColumns = table.columns.indices.toList.map { i =>
          val attr = ResolvedTableAttribute(table, id, i)
          ProjectionColumn(attr, None)
        }
        Some(projectionColumns)
      }

    def ifQualifierMatches[A](qualifier: Option[String])(body: => Option[A]): Option[A] = {
      if (qualifier.isEmpty || qualifier == Some(table.name)) body else None
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case rta @ ResolvedTableAttribute(`table`, `id`, index) =>
        Some(Binding(scope.name, List(Field(rta.name))))
      case ResolvedProjectedAttribute(parent: ResolvedAttribute, _, _) =>
        lookup(parent)
      case _ => None
    }

    override def toString: String = s"TableContext(${table.name}{$id})"
  }

  case class JoinContext(outer: Context, inner: Context) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      (outer.resolveColumn(ref), inner.resolveColumn(ref)) match {
        case (Some(b), None) =>
          Some(b)
        case (None, Some(b)) =>
          Some(b)
        case (Some(_), Some(_)) =>
          throw SqlException(s"Ambiguous reference to $ref")
        case _ => None
      }
    }

    def resolveStar(qualifier: Option[String]) =
      (outer.resolveStar(qualifier), inner.resolveStar(qualifier)) match {
        case (None, None) =>
          None
        case (leftAttrs, rightAttrs) =>
          Some(leftAttrs.getOrElse(Nil) ++ rightAttrs.getOrElse(Nil))
      }

    def lookup(ref: ResolvedAttribute): Option[Binding] = {
      (outer.lookup(ref), inner.lookup(ref)) match {
        case (Some(b), None) =>
          val b1 = b.copy(path = First :: b.path)
          Some(b1)
        case (None, Some(b)) =>
          val b1 = b.copy(path = Second :: b.path)
          Some(b1)
        case (Some(_), Some(_)) =>
          throw SqlException(s"Ambiguous reference to $ref")
        case _ => None
      }
    }
  }

  case class AliasContext(parent: Context, alias: String) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] =
      ref.table match {
        case None =>
          parent.resolveColumn(ref)
        case Some(`alias`) =>
          parent.resolveColumn(UnresolvedAttribute(None, ref.name))
        case _ =>
          None
      }

    def resolveStar(qualifier: Option[String]) = qualifier match {
      case None | Some(`alias`) =>
        parent.resolveStar(None)
      case _ =>
        parent.resolveStar(qualifier)
    }

    def lookup(resolved: ResolvedAttribute): Option[Binding] =
      parent.lookup(resolved)
  }

  case class ProjectContext(parent: Context, columns: List[ProjectionColumn]) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = {
      val qualifier = ref.table
      val name = ref.name

      @tailrec
      def matchColumn(expr1: Expression): Boolean = expr1 match {
        case ResolvedProjectedAttribute(_, Some(name1), _) if name == name1 && qualifier.isEmpty =>
          true
        case ResolvedProjectedAttribute(parentExpr, _, _) =>
          matchColumn(parentExpr)
        case tableAttribute: ResolvedTableAttribute =>
          tableAttribute.name == name && qualifier.fold(true)(_ == tableAttribute.table.name)
        case _ => false
      }

      columns.indexWhere {
        case ProjectionColumn(expr, alias) =>
          (alias == Some(name) && qualifier.isEmpty) || matchColumn(expr)
      } match {
        case -1 =>
          None
        case i =>
          val savedScope = currScope
          currScope = scope.copy(ctx = parent)
          val expr = columns(i).expr
          val attr = ResolvedProjectedAttribute(expr, Some(name), i)
          currScope = savedScope
          Some(attr)
      }
    }

    def resolveStar(qualifier: Option[String]) =
      if (qualifier.isDefined)
        parent.resolveStar(qualifier)
      else
        Some(columns)

    def lookup(resolved: ResolvedAttribute): Option[Binding] = resolved match {
      case ResolvedProjectedAttribute(_, _, i) =>
        Some(Binding(scope.name, List(Index(i))))
      case rta: ResolvedTableAttribute =>
        columns.indexWhere(_.expr == rta) match {
          case -1 =>
            None
          case i =>
            Some(Binding(scope.name, List(Index(i))))
        }
    }
  }

  case class AggregateContext(parent: Context, groupedBy: List[Expression], aggregates: List[AggregateExpr]) extends Context {
    def resolveColumn(ref: UnresolvedAttribute): Option[ResolvedAttribute] = None

    override def resolveAggregate(agg: AggregateExpr): Option[ResolvedAttribute] =
      aggregates.indexOf(agg) match {
        case -1 =>
          None
        case i =>
          Some(ResolvedProjectedAttribute(agg, None, i))
      }

    // should this throw an exception instead?
    def resolveStar(qualifier: Option[String]) = None

    override def lookup(resolved: ResolvedAttribute): Option[Binding] =
      groupedBy.indexOf(resolved) match {
        case -1 =>
          resolved match {
            case ResolvedProjectedAttribute(agg: AggregateExpr, _, i) =>
              val path = if (groupedBy.nonEmpty) List(Index(1), Index(i)) else List(Index(i))
              Some(Binding(scope.name, path))
            case _ => None
          }
        case i =>
          Some(Binding(scope.name, List(Index(0), Index(i))))
      }
  }

  def buildContext(op: Operator): Context = {
    op match {
      case Join(left, right, _, _) => JoinContext(buildContext(left), buildContext(right))
      case Scan(t, id) => TableContext(table(t), id)
      case OrderBy(p, by) => buildContext(p)
      case GroupBy(p, by) => buildContext(p) // FIXME remove
      case Filter(p, predicate) => buildContext(p)
      case Project(p, columns) =>
        val ctx1 = buildContext(p)
        val columns1 = columns.flatMap {
          case col: ProjectionColumn =>
            List(col)
          case star @ UnresolvedStar(qualifier) =>
            ctx1.resolveStar(qualifier).getOrElse {
              throw new SqlException(s"Can't resolve $star")
            }
        }
        ProjectContext(ctx1, columns1)
      case Aggregate(p, groupedBy, aggregates) => AggregateContext(buildContext(p), groupedBy, aggregates)
      case TableAlias(p, a) => AliasContext(buildContext(p), a)
      case SubSelect(p) => buildContext(p)
      case _ => throw new NotImplementedError(s"buildContext($op)")
    }
  }

  // op is unused here, but may be used in overrides (some DBs use + for string concatenation)
  def commonArithmeticType(op: ArithOp, left: ColumnType, right: ColumnType): ColumnType = {
    if (left == right)
      left
    else if (left == DoubleType || right == DoubleType)
      DoubleType
    else if (left == BigIntType || right == BigIntType)
      BigIntType
    else if (left == IntType || right == IntType)
      IntType
    else if (left.isInstanceOf[StringType] || right.isInstanceOf[StringType] || left == AnyType || right == AnyType)
      AnyNumberType
    else throw SqlException("Incompatible types " + left.sqlName + " and " + right.sqlName)
  }

  def getExprType(expr: Expression): ColumnType = {
    expr match {
      case BinOpExpr(op, l, r) =>
        op match {
          case _: LogicOp | _: ComparisonOp =>
            BoolType
          case op: ArithOp =>
            commonArithmeticType(op, getExprType(l), getExprType(r))
          case Concat => BasicStringType
        }
      case LikeExpr(l, r, escape) => BoolType
      case InExpr(l, r) => BoolType
      case ExistsExpr(_) => BoolType
      case NegExpr(opd) => getExprType(opd)
      case NotExpr(_) => BoolType
      case AggregateExpr(Count, _, _) => IntType
      case AggregateExpr(Avg, _, _) => DoubleType
      case AggregateExpr(Sum | Max | Min, _, opd) => getExprType(opd)
      case SubstrExpr(str, from, len) => BasicStringType
      case CaseWhenExpr(list) => getExprType(list(1))
      case Literal(v, t) => t
      case CastExpr(e, t) => t
      case SelectExpr(s) => DoubleType
      case FuncExpr(name, args) => funcType(name, args)
      case Parameter() => AnyType
      case tableAttribute: ResolvedTableAttribute => tableAttribute.sqlType
      case ResolvedProjectedAttribute(parent, _, _) =>
        getExprType(parent)
      case c: UnresolvedAttribute =>
        throw new SqlException("getExprType is only called for resolved expressions")
      case _ => throw new NotImplementedError(s"getExprType($expr)")
    }
  }

  /** Returns the type of `FuncExpr(name, args)`. Override if the type depends on args, use
    * `registerFunctionType` otherwise. Default is `DoubleType`
    */
  def funcType(name: String, args: List[Expression]) = name match {
    case "abs" | "trunc" | "truncate" | "round" | "power" | "mod" | "sign" | "ceiling" | "ceil" | "floor" | "nullif" =>
      getExprType(args.head)
    case "coalesce" =>
      // TODO behavior may be wrong in some cases, but we don't support nulls yet anyway
      args.map(getExprType).reduce(commonArithmeticType(Minus, _, _))
    case _ =>
      funcTypes.getOrElse(name,
        throw new IllegalArgumentException(s"Unknown return type for $name(${args.mkString(", ")}). Override `SqlCompiler.funcType` or call `registerFunctionType` if the type doesn't depend on arguments"))
  }

  def registerFunctionType(name: String, tpe: ColumnType) =
    funcTypes.update(name, tpe)

  private val funcTypes = collection.mutable.Map.empty[String, ColumnType]

  // https://en.wikibooks.org/wiki/SQL_Dialects_Reference
  Seq(
    IntType -> Seq(
      "length", "bit_length", "char_length", "octet_length", "width_bucket", "ascii", "instr"
    ),
    DoubleType -> Seq(
      "asin", "acos", "atan", "atan2", "sin", "cos", "tan", "cot", "sinh", "cosh", "tanh", "atanh",
      "sqrt", "exp", "ln", "log", "log10", "rand"
    ),
    BasicStringType -> Seq(
      "chr", "char", "concat", "hex", "lower", "upper", "lcase", "ucase", "lpad", "rpad",
      "trim", "ltrim", "rtrim", "left", "right", "repeat", "reverse", "space", "substring", "substr",
      "replace", "initcap", "translate", "quote", "soundex", "md5", "sha1"
    ),
    DateType -> Seq("current_date", "date"),
    TimeType -> Seq("current_time", "time"),
    TimestampType -> Seq("current_timestamp", "now")
  ).foreach {
    case (tpe, funs) => funs.foreach(registerFunctionType(_, tpe))
  }

  def containsAggregates(selectListElement: SelectListElement): Boolean = selectListElement match {
    case column: ProjectionColumn =>
      containsAggregates(column.expr)
    case _: UnresolvedStar =>
      false
  }

  def containsAggregates(expr: Expression): Boolean =
    expr match {
      case _: AggregateExpr =>
        true
      case BinOpExpr(_, l, r) =>
        containsAggregates(l) || containsAggregates(r)
      case NegExpr(opd) =>
        containsAggregates(opd)
      case NotExpr(opd) =>
        containsAggregates(opd)
      case _ => false
    }

  def depends(on: Operator, subquery: Operator): Boolean = subquery match {
    case Join(left, right, _, _) => depends(on, left) || depends(on, right)
    case Scan(t, _) => false
    case OrderBy(p, by) => depends(on, p) || depends(on, by.map(_.expr))
    case Aggregate(p, groupedBy, columns) => depends(on, p) || depends(on, groupedBy) || depends(on, columns)
    case Filter(p, predicate) => depends(on, p) || depends(on, predicate)
    case Project(p, columns) => depends(on, p) || depends(on, columns.map {
      case col: ProjectionColumn => col.expr
      case _: UnresolvedStar => throw new SqlException(s"$subquery has unresolved expression")
    })
    case TableAlias(t, a) => depends(on, t)
    case SubSelect(p) => depends(on, p)
    case _ => false
  }

  def depends(op: Operator, list: ExprList): Boolean = list.exists(e => depends(op, e))

  def depends(op: Operator, expr: Expression): Boolean = {
    expr match {
      case BinOpExpr(_, l, r) => depends(op, l) || depends(op, r)
      case ExistsExpr(q) => depends(op, q)
      case LikeExpr(l, r, escape) =>
        depends(op, l) || depends(op, r) || escape.exists(depends(op, _))
      case NegExpr(opd) => depends(op, opd)
      case NotExpr(opd) => depends(op, opd)
      case _: Literal | _: Parameter => false
      case CastExpr(exp, typ) => depends(op, exp)
      case resolved: ResolvedAttribute => buildContext(op).lookup(resolved).isDefined
      case SelectExpr(s) => depends(op, s)
      case AggregateExpr(_, _, opd) => depends(op, opd)
      case SubstrExpr(str, from, len) => depends(op, str) || depends(op, from) || depends(op, len)
      case CaseWhenExpr(list) => depends(op, list)
      case InListExpr(sel, lst) => depends(op, sel) || depends(op, lst)
      case InExpr(sel, query) => depends(op, sel) || depends(op, query)
      case FuncExpr(name, args) => depends(op, args)
      case _ =>
        throw new SqlException(s"Can't check if $op depends on $expr")
    }
  }
}
