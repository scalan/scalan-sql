package scalan.sql.parser

import SqlAST.{BasicStringType, Schema}

class SqliteResolver(schema: Schema) extends SqlResolver(schema) {
  registerFunctionType("strftime", BasicStringType)
}
