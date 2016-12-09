package scalan.sql

import scalan.sql.parser.SqlAST._

case class ColumnUseInfo(constraints: ConstraintSet,
                         orderBy: SqlOrdering,
                         groupBy: List[ResolvedTableAttribute],
                         optAttributeOrder: Option[List[ResolvedTableAttribute]]) {
  def forScan(scan: Scan) = {
    def includeAttribute(attribute: ResolvedTableAttribute) =
      attribute.table.name == scan.tableName && attribute.tableId == scan.id

    val constraints1 = constraints.asMap.collect { case (attr, v) if includeAttribute(attr) => (attr.name, v) }
    val orderBy1 = orderBy.collect { case SortSpec(attr: ResolvedTableAttribute, dir, nulls) if includeAttribute(attr) => (attr.name, dir, nulls) }
    val groupBy1 = groupBy.filter(includeAttribute).map(_.name)
    val attributeOrder1 = optAttributeOrder.filter(_.forall(includeAttribute))
    SingleTableColumnUseInfo(constraints1, orderBy1, groupBy1, attributeOrder1)
  }

  def addConstraints(predicate: Expression) =
    copy(constraints = constraints.addConstraints(predicate))
}
object ColumnUseInfo {
  def apply(): ColumnUseInfo = ColumnUseInfo(ConstraintSet.empty, Nil, Nil, None)
}

case class SingleTableColumnUseInfo(constraints: Map[String, Map[ComparisonOp, Set[Expression]]],
                                    orderBy: List[(String, SortDirection, NullsOrdering)],
                                    groupBy: List[String],
                                    optAttributeOrder: Option[List[ResolvedTableAttribute]])
