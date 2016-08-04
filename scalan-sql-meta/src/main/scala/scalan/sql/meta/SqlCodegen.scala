package scalan.sql.meta

import scalan.meta.ScalanAst.{SApply, SConst, SMethodDef, STraitCall}
import scalan.sql.parser.SqlAST._
import scalan.sql.parser.SqlResolver

class SqlCodegen extends SqlResolver("") {
  var indent = "\t"

  def generateSchema(sql: String): String = {
    val statements = parseDDL(sql)

    statements.map {
      case CreateIndexStmt(i) => generateIndex(i)
      case CreateTableStmt(t) => generateTable(t)
      case s => throw new NotImplementedError(s"Cannot generate schema for statement $s")
    }.mkString("\n\n")
  }

  var currMethod: SMethodDef = null

  def generateQuery(sql: String, m: SMethodDef): String = {
    val args = m.allArgs.map(arg => arg.name + ": " + arg.tpe).mkString(", ")
    val select = parseSelect(sql)
    val op = select.operator
    currMethod = m
    s"""    type ${m.name}_Result = ${resultType(select)}
       |
       |    override def ${m.name}(${args}) = ${generateOperator(op)}${tableToArray(op)}""".stripMargin
  }

  def divOp(expr: Expression) = if (getExprType(expr) == IntType) "/!" else "/"

  def patternMatch(text: Expression, pattern: Expression, escape: Option[Expression]): String = {
    val left = generateExpr(text)
    pattern match {
      case Literal(v, t) if (t.isInstanceOf[StringType]) =>
        assert(escape.isEmpty, "escape currently not supported")
        val p = v.toString
        if (p.indexOf('%') < 0 && p.indexOf('_') < 0) "(" + left + " == \"" + p + "\")"
        else if (p.lastIndexOf('%') == 0 && p.indexOf('_') < 0) left + ".startsWith(\"" + p.substring(1) + "\")"
        else if (p.indexOf('%') == p.length - 1 && p.indexOf('_') < 0) left + ".endsWith(\"" + p.substring(0, p.length - 1) + "\")"
        else if (p.lastIndexOf('%', p.length - 2) == 0 && p.indexOf('%', 1) == p.length - 1 && p.indexOf('_') < 0) left + ".contains(\"" + p.substring(1, p.length - 1) + "\")"
        else left + ".matches(\"" + p.replace("%", ".*").replace('_', '.') + "\")"
    }
  }

  def generateCaseWhen(list: ExprList, i: Int): String = {
    if (i == list.length) ""
    else if (i == list.length - 1) " ELSE (" + generateExpr(list(i)) + ")"
    else (if (i == 0) "IF (" else " ELSEIF (") + generateExpr(list(i)) + ") THEN (" + generateExpr(list(i + 1)) + ")" + generateCaseWhen(list, i + 2)
  }

  def printValue(value: Any, tp: ColumnType): String = {
    if (tp.isInstanceOf[StringType]) "\"" + value.toString + "\""
    else value.toString
  }

  def generateExpr(expr: Expression): String = {
    expr match {
      case BinOpExpr(op, l, r) =>
        // Ignore behavior on nulls currently
        val opStr = op match {
          case And => "&&"
          case Or => "||"
          case Plus => "+"
          case Minus => "-"
          case Times => "*"
          case Divide => divOp(expr)
          case Modulo => "%"
          case Is => "==="
          case Eq => "==="
          case Less => "<"
          case LessEq => "<="
          case Greater => ">"
          case GreaterEq => ">="
          case Concat => "concat"
        }
        val lCode = generateExpr(l)
        val rCode = generateExpr(r)
        s"($lCode $opStr $rCode)"
      case ExistsExpr(q) => "(" + generateOperator(q) + ".count !== 0)"
      case LikeExpr(l, r, escape) =>
        patternMatch(l, r, escape)
      case NegExpr(opd) => "-" + generateExpr(opd)
      case NotExpr(opd) => "!" + generateExpr(opd)
      case Literal(v, t) => "toRep(" + printValue(v, t) + ")"
      case CastExpr(exp, typ) => generateExpr(exp) + (if (typ.isInstanceOf[StringType]) ".toStr" else ".to" + typ.scalaName)
      case c: ColumnRef => {
        val binding = lookup(c)
        binding.asScalaCode
      }
      case SelectExpr(s) => {
        val saveIndent = indent
        indent += "\t"
        val subselect = "(" + generateOperator(s) + ")"
        indent = saveIndent
        subselect
      }
      // TODO currently ignores distinct
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count => "result.count"
          case Avg => "result.avg(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Sum => "result.sum(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Max => "result.max(" + currScope.name + " => " + generateExpr(opd) + ")"
          case Min => "result.min(" + currScope.name + " => " + generateExpr(opd) + ")"
        }
      case SubstrExpr(str, from, len) => generateExpr(str) + ".substring(" + generateExpr(from) + ", " + generateExpr(from) + " + " + generateExpr(len) + ")"
      case CaseWhenExpr(list) => generateCaseWhen(list, 0)
      case InListExpr(sel, lst) => "(" + lst.map(alt => (generateExpr(sel) + " === " + generateExpr(alt))).mkString(" || ") + ")"
      case InExpr(sel, query) => {
        val saveIndent = indent
        indent += "\t"
        val subselect = "(" + generateOperator(query) + ".where(e => e == " + generateExpr(sel) + ").count !== 0)"
        indent = saveIndent
        subselect
      }
      case FuncExpr(name, args) => name + "(" + args.map(p => generateExpr(p)).mkString(", ") + ")"
      case _ => throw new NotImplementedError(s"generateExpr($expr)")
    }
  }

  def buildTree(elems: Seq[String], lpar: String = "(", rpar: String = ")", i: Int = 0): String = {
    val n = elems.length
    if (i < n - 2) lpar + elems(i) + ", " + buildTree(elems, lpar, rpar, i + 1)
    else if (n >= 2) lpar + elems(i) + ", " + elems(i + 1) + (rpar * (n - 1))
    else if (n != 0) elems(i)
    else "()"
  }

  def generateExprList(list: ExprList): String = buildTree(list.map(expr => generateExpr(expr)), "Pair(")

  def resolveKey(key: Expression): Option[Binding] = key match {
    case ref: ColumnRef => currScope.ctx.resolve(ref)
    case _ => throw SqlException("Unsupported join condition")
  }

  def extractKey(spec: JoinSpec): String = spec match {
    case On(on) => extractKey(on)
    case _ => throw new NotImplementedError(s"extractKey($spec)")
  }

  def extractKey(on: Expression): String = {
    on match {
      case BinOpExpr(And, l, r) => "Pair(" + extractKey(l) + ", " + extractKey(r) + ")"
      case BinOpExpr(Eq, l, r) => (resolveKey(l), resolveKey(r)) match {
        case (Some(_), Some(_)) => throw SqlException("Ambiguous reference to column")
        case (Some(b), None) => b.asScalaCode
        case (None, Some(b)) => b.asScalaCode
        case (None, None) => throw SqlException("Failed to locate column in join condition")
      }
      case _ => throw new NotImplementedError(s"extractKey($on)")
    }
  }

  def generateJoinKey(table: Operator, spec: JoinSpec): String = {
    pushContext(table)
    val result = currScope.name + " => " + extractKey(spec)
    popContext()
    result
  }

  def generateLambdaExpr(table: Operator, exp: Expression): String = {
    pushContext(table)
    val result = currScope.name + " => " + generateExpr(exp)
    popContext()
    result
  }

  def generateLambdaExprList(table: Operator, exps: ExprList): String = {
    pushContext(table)
    val result = currScope.name + " => " + generateExprList(exps)
    popContext()
    result
  }

  def generateAggOperand(agg: Expression): String = {
    agg match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count => "1"
          case _ => generateExpr(opd)
        }
      case _ => throw new NotImplementedError(s"generateAggOperand($agg)")
    }
  }

  def aggCombine(agg: Expression, s1: String, s2: String): String = {
    agg match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count | Avg | Sum =>
            s"""$s1 + $s2"""
          case Max =>
            s"""if ($s1 > $s2) $s1 else $s2"""
          case Min =>
            s"""if ($s1 < $s2) $s1 else $s2"""
        }
      case _ => throw new NotImplementedError(s"aggCombine($agg, $s1, $s2)")
    }
  }

  def getAggPath(columns: List[ProjectionColumn], length: Int, n: Int): String = {
    var aggIndex = 0
    for (i <- 0 until n) {
      if (isAggregate(columns(i))) aggIndex += 1
    }
    pathString("tail" :: indexToPath(aggIndex, length))
  }

  def generateAggResult(columns: List[ProjectionColumn], length: Int, gby: ExprList, i: Int, count: Int, expr: Expression): String = {
    lazy val aggPath = getAggPath(columns, length, i)
    expr match {
      case AggregateExpr(op, _, opd) =>
        op match {
          case Count | Sum | Max | Min =>
            aggPath
          case Avg =>
            val countPath = pathString("tail" :: indexToPath(count, length))
            s"""($aggPath.toDouble / $countPath.toDouble)"""
        }
      case c: ColumnRef => {
        val keyIndex = gby.indexWhere {
          k => matchExpr(c, None, k)
        }
        if (keyIndex < 0)
          throw SqlException("Unsupported group-by clause")
        else
          pathString("head" :: indexToPath(keyIndex, gby.length))
      }
      case _ => throw new NotImplementedError(s"generateAggResult($columns, $length, $gby, $i, $count)")
    }
  }

  def groupBy(agg: Operator, gby: ExprList): String = {
    agg match {
      case Project(p, columns) => {
        var aggregates = columns.filter(isAggregate)
        var countIndex = aggregates.indexWhere {
          case ProjectionColumn(CountAllExpr, _) => true
          case _ => false
        }
        if (countIndex < 0 && aggregates.exists {
          case ProjectionColumn(AggregateExpr(Avg, _, _), _) => true
          case _ => false
        }) {
          countIndex = aggregates.length
          aggregates = aggregates :+ ProjectionColumn(CountAllExpr, None)
        }
        val numberOfAggregates = aggregates.length
        pushContext(p)
        val aggTypes = buildTree(aggregates.map(agg => getExprType(agg.expr).scalaName))
        val groupBy = buildTree(gby.map(col => {
          val binding = lookup(ref(col))
          binding.asScalaCode
        }), "Pair(")
        val map = buildTree(aggregates.map(agg => generateAggOperand(agg.expr)), "Pair(")
        val reduce = if (numberOfAggregates == 1)
          aggCombine(aggregates(0).expr, "s1", "s2")
        else
          aggregates.indices.map(i => aggCombine(aggregates(i).expr, "s1._" + (i + 1), "s2._" + (i + 1))).mkString(",")
        val aggResult = buildTree(columns.indices.map(i => generateAggResult(columns, numberOfAggregates, gby, i, countIndex, columns(i).expr)), "Pair(")
        val result =
          s"""ReadOnlyTable(${generateOperator(p)}
              |$indent.mapReduce(${currScope.name} => Pair(${groupBy}, ${map}),
              |$indent\t(s1: Rep[${aggTypes}], s2: Rep[${aggTypes}]) => (${reduce})).toArray.map(${currScope.name} => ${aggResult}))""".stripMargin
        popContext()
        result
      }
      case _ => throw SqlException("Unsupported group-by clause")
    }
  }

  def generateOperator(op: Operator): String = {
    op match {
      case Join(outer, inner, joinType, spec) =>
        generateOperator(outer) + s"""\n$indent.join(${generateOperator(inner)}, $joinType)(${generateJoinKey(outer, spec)}, ${generateJoinKey(inner, spec)})"""
      case Scan(t) =>
        currMethod.explicitArgs.find(arg => arg.tpe match {
          case STraitCall(n1, List(STraitCall(n2, List(p)))) if n1 == "Rep" && n2 == "Table" && p.toString == t.capitalize => true
          case _ => false
        }) match {
          // TODO: global lookup
          case Some(arg) => arg.name
          case _ => t.toLowerCase
        }
      case OrderBy(p, by) =>
        // TODO ignores null ordering
        val expressions = by.map {
          case SortSpec(expr, Ascending, _) =>
            expr
          case SortSpec(expr, Descending, _) =>
            // TODO doesn't handle strings properly, but this requires changes in scalan-sql
            NegExpr(expr)
        }
        generateOperator(p) + s"""\n$indent.orderBy(${generateLambdaExprList(p, expressions)})"""
      case GroupBy(p, by) => groupBy(p, by)
      case Filter(p, predicate) => {
        val (joins, conjuncts) = optimize(p, predicate)
        conjuncts match {
          case Literal(_, _) => generateOperator(joins)
          case _ => generateOperator(joins) + s"""\n$indent.where(${generateLambdaExpr(p, conjuncts)})"""
        }
      }
      case Project(p, columns) =>
        if (isGrandAggregate(columns)) {
          pushContext(p)
          val agg = s"""{ val result = ${generateOperator(p)} ; (${generateExprList(columns.map(_.expr))}) }"""
          popContext()
          agg
        } else {
          generateOperator(p) + s"""\n$indent.select(${generateLambdaExprList(p, columns.map(_.expr))})"""
        }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) =>
        val saveIdent = indent
        indent = indent + "\t"
        val subquery = generateOperator(p)
        indent = saveIdent
        subquery
      case _ => throw new NotImplementedError(s"generateOperator($op)")
    }
  }

  def tableToArray(op: Operator): String = {
    op match {
      case OrderBy(p, by) => ""
      case Project(p, c) if isGrandAggregate(c) => ""
      case _ => ".toArray"
    }
  }

  def operatorType(op: Operator): String = {
    op match {
      case Join(outer, inner, _, _) => "(" + operatorType(outer) + ", " + operatorType(inner) + ")"
      case Scan(t) => buildTree(table(t).columns.map(_.ctype.scalaName))
      case OrderBy(p, by) => operatorType(p)
      case GroupBy(p, by) => operatorType(p)
      case Filter(p, predicate) => operatorType(p)
      case Project(p, columns) => {
        pushContext(p)
        val projection = buildTree(columns.map(c => getExprType(c.expr).scalaName))
        popContext()
        projection
      }
      case TableAlias(t, a) => generateOperator(t)
      case SubSelect(p) => operatorType(p)
      case _ => throw new NotImplementedError(s"operatorType($op)")
    }
  }

  def resultType(query:
                 SelectStmt): String = {
    query.operator match {
      case OrderBy(p, by) => "Arr[" + operatorType(p) + "]"
      case Project(p, c) if isGrandAggregate(c) => "Rep[" + operatorType(p) + "]"
      case _ => "Arr[" + operatorType(query.operator) + "]"
    }
  }

  def parseType(t: ColumnType): String = {
    t match {
      case StringType(_, _) => ""
      case DateType => ".toDate"
      case _ => ".to" + t.scalaName
    }
  }

  def generateTable(table: Table): String = {
    val columns = table.columns
    val n_columns = columns.length
    val typeName = table.name.capitalize
    //    val typeDef = buildTree(columns.map(c => c.ctype.scalaName))
    val classDef = Array.tabulate(n_columns)(i => s"  def ${columns(i).name} = ${pathString("self", indexToPath(i, n_columns))}").mkString("\n")
    val parse = buildTree(Array.tabulate(n_columns)(i => "c(" + i + ")" + parseType(columns(i).ctype)), "Pair(")
    val pairTableDef = buildTree(columns.map(c => "Table.create[" + c.ctype.scalaName + "](tableName + \"." + c.name + "\")"), "PairTable.create(")
    s"""
       |    def create$typeName(tableName: Rep[String]) =
       |      $pairTableDef
       |
       |    def parse$typeName(c: Arr[String]): Rep[${typeName}Data] =
       |      $parse
       |
       |""".stripMargin
  }

  def generateIndex(index: Index): String = {
    currScope = Scope(TableContext(table(index.tableName)), Some(currScope), 0, "r")
    val body = buildTree(index.columns.map(part => pathString(lookup(ColumnRef(None, part.name)).path)), "Pair(")
    val result = s"""def ${index.name}(${currScope.name}: Rep[${index.tableName.capitalize}]) = $body"""
    popContext()
    result
  }
}
