package scalan.sql

case class TestQuery(sql: String, accessedColumns: Map[String, Set[String]] = Map.empty)

object TestQuery {
  implicit def fromSql(sql: String): TestQuery = TestQuery(sql)
}
