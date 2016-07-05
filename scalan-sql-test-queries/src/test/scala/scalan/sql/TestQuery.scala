package scalan.sql

case class TestQuery(sql: String, accessedColumns: Map[String, Set[String]] = Map.empty)
