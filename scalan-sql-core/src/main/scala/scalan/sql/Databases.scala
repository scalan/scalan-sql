package scalan.sql

import java.lang.reflect.Method

import scalan._
import scalan.sql.parser.SqlAST.{Index, SortDirection, Table}

trait Databases extends ScalanDsl {
  self: DatabasesDsl with ScalanSql =>

  type RDatabase[S <: SSchema] = Rep[Database[S]]

  trait Database[S <: SSchema] extends Def[Database[S]] {
    def eS: Elem[S]
  }

  abstract class DatabaseImpl[S <: SSchema](val connectionString: Rep[String])(implicit val eS: Elem[S]) extends Database[S]
}

trait DatabasesDsl extends impl.DatabasesAbs { self: ScalanSql =>
  implicit def DatabaseElemExtensions[A <: SSchema](ie: Elem[Database[A]]) = ie.asInstanceOf[DatabaseElem[A, Database[A]]]

  implicit class DbOps[S <: SSchema](db: RDatabase[S]) {
    def schema = db.selfType1.eS.asInstanceOf[SingletonElem[S]].value

    def sTable(name: String) = schema.tables.find(_.name == name).getOrElse {
      !!!(s"No table $name in database $db")
    }

    // return scannables by name: need to decide how more precisely
  }

  // Scalan representations of SqlAST.{Table, Index, Schema}
  // what to do about indexes on expressions?
  case class SIndex(name: String, columns: List[(String, SortDirection)], isUnique: Boolean, isPrimaryKey: Boolean)

  case class STable(name: String, columns: List[(String, Elem[_])], indices: List[SIndex]) {
    lazy val rowElem = structElement(columns)
  }

  case class SSchema(tables: List[STable])

  def database(schema: SSchema, connectionString: String = "") =
    DatabaseImpl(toRep(connectionString))(SingletonElem[schema.type](schema))
}

trait DatabasesDslStd extends impl.DatabasesStd { self: ScalanSqlStd =>
}

trait DatabasesDslExp extends impl.DatabasesExp { self: ScalanSqlExp =>
  override def getResultElem(receiver: Exp[_], m: Method, args: List[AnyRef]) = receiver.elem match {
//    case elem: DatabaseElem[_, _] =>
//      m.getName match {
//        case "fullScan" | "search" =>
//          iterElement(elem.eRow)
//        case _ => super.getResultElem(receiver, m, args)
//      }
    case _ =>
      super.getResultElem(receiver, m, args)
  }
}
