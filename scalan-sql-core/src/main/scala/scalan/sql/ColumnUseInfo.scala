package scalan.sql

import scalan.sql.parser.SqlAST._

case class ColumnUseInfo(constraints: Map[ResolvedTableAttribute, Set[(ComparisonOp, Expression)]],
                         orderBy: List[(ResolvedTableAttribute, SortDirection)],
                         groupBy: Set[ResolvedTableAttribute]) {
  def forScan(scan: Scan) = {
    def includeAttribute(attribute: ResolvedTableAttribute) =
      attribute.table.name == scan.tableName && attribute.tableId == scan.id

    val constraints1 = constraints.collect { case (attr, v) if includeAttribute(attr) => (attr.name, v) }
    val orderBy1 = orderBy.collect { case (attr, dir) if includeAttribute(attr) => (attr.name, dir) }
    val groupBy1 = groupBy.filter(includeAttribute).map(_.name)
    SingleTableColumnUseInfo(constraints1, orderBy1, groupBy1)
  }
}
object ColumnUseInfo {
  def apply(): ColumnUseInfo = ColumnUseInfo(Map.empty, Nil, Set.empty)
}

case class SingleTableColumnUseInfo(constraints: Map[String, Set[(ComparisonOp, Expression)]],
                                    orderBy: List[(String, SortDirection)],
                                    groupBy: Set[String])