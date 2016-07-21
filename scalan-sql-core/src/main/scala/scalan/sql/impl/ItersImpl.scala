package scalan.sql

import java.lang.reflect.Method
import scalan._
import scalan.common.Lazy
import scala.reflect.runtime.universe._
import scalan.common.Default

package impl {
// Abs -----------------------------------
trait ItersAbs extends Scalan with Iters {
  self: ItersDsl with ScalanSql =>

  // single proxy for each type family
  implicit def proxyIter[Row](p: Rep[Iter[Row]]): Iter[Row] = {
    proxyOps[Iter[Row]](p)(scala.reflect.classTag[Iter[Row]])
  }

  // familyElem
  class IterElem[Row, To <: Iter[Row]](implicit _eRow: Elem[Row])
    extends EntityElem[To] {
    def eRow = _eRow
    lazy val parent: Option[Elem[_]] = None
    lazy val typeArgs = TypeArgs("Row" -> eRow)
    override def isEntityType = true
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[Iter[Row]].asInstanceOf[WeakTypeTag[To]]
    }
    override def convert(x: Rep[Def[_]]) = {
      implicit val eTo: Elem[To] = this
      val conv = fun {x: Rep[Iter[Row]] => convertIter(x) }
      tryConvert(element[Iter[Row]], this, x, conv)
    }

    def convertIter(x: Rep[Iter[Row]]): Rep[To] = {
      x.selfType1 match {
        case _: IterElem[_, _] => x.asRep[To]
        case e => !!!(s"Expected $x to have IterElem[_, _], but got $e", x)
      }
    }

    override def getDefaultRep: Rep[To] = ???
  }

  implicit def iterElement[Row](implicit eRow: Elem[Row]): Elem[Iter[Row]] =
    cachedElem[IterElem[Row, Iter[Row]]](eRow)

  implicit case object IterCompanionElem extends CompanionElem[IterCompanionAbs] {
    lazy val tag = weakTypeTag[IterCompanionAbs]
    protected def getDefaultRep = Iter
  }

  abstract class IterCompanionAbs extends CompanionDef[IterCompanionAbs] with IterCompanion {
    def selfType = IterCompanionElem
    override def toString = "Iter"
  }
  def Iter: Rep[IterCompanionAbs]
  implicit def proxyIterCompanionAbs(p: Rep[IterCompanionAbs]): IterCompanionAbs =
    proxyOps[IterCompanionAbs](p)

  abstract class AbsTableIter[Row]
      ()(implicit eRow: Elem[Row], tableName: SingletonElem[String])
    extends TableIter[Row]() with Def[TableIter[Row]] {
    lazy val selfType = element[TableIter[Row]]
  }
  // elem for concrete class
  class TableIterElem[Row](val iso: Iso[TableIterData[Row], TableIter[Row]])(implicit override val eRow: Elem[Row], val tableName: SingletonElem[String])
    extends IterElem[Row, TableIter[Row]]
    with ConcreteElem[TableIterData[Row], TableIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertIter(x: Rep[Iter[Row]]) = TableIter()
    override def getDefaultRep = TableIter()
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIter[Row]]
    }
  }

  // state representation type
  type TableIterData[Row] = Unit

  // 3) Iso for concrete class
  class TableIterIso[Row](implicit eRow: Elem[Row], tableName: SingletonElem[String])
    extends EntityIso[TableIterData[Row], TableIter[Row]] with Def[TableIterIso[Row]] {
    override def from(p: Rep[TableIter[Row]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      TableIter()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new TableIterElem[Row](self)
    lazy val selfType = new TableIterIsoElem[Row](eRow, tableName)
    def productArity = 2
    def productElement(n: Int) = (eRow, tableName).productElement(n)
  }
  case class TableIterIsoElem[Row](eRow: Elem[Row], tableName: SingletonElem[String]) extends Elem[TableIterIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new TableIterIso[Row]()(eRow, tableName))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[TableIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class TableIterCompanionAbs extends CompanionDef[TableIterCompanionAbs] {
    def selfType = TableIterCompanionElem
    override def toString = "TableIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[TableIterData[Row]])(implicit eRow: Elem[Row], tableName: SingletonElem[String]): Rep[TableIter[Row]] =
      isoTableIter(eRow, tableName).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row]()(implicit eRow: Elem[Row], tableName: SingletonElem[String]): Rep[TableIter[Row]] =
      mkTableIter()

    def unapply[Row](p: Rep[Iter[Row]]) = unmkTableIter(p)
  }
  lazy val TableIterRep: Rep[TableIterCompanionAbs] = new TableIterCompanionAbs
  lazy val TableIter: TableIterCompanionAbs = proxyTableIterCompanion(TableIterRep)
  implicit def proxyTableIterCompanion(p: Rep[TableIterCompanionAbs]): TableIterCompanionAbs = {
    proxyOps[TableIterCompanionAbs](p)
  }

  implicit case object TableIterCompanionElem extends CompanionElem[TableIterCompanionAbs] {
    lazy val tag = weakTypeTag[TableIterCompanionAbs]
    protected def getDefaultRep = TableIter
  }

  implicit def proxyTableIter[Row](p: Rep[TableIter[Row]]): TableIter[Row] =
    proxyOps[TableIter[Row]](p)

  implicit class ExtendedTableIter[Row](p: Rep[TableIter[Row]])(implicit eRow: Elem[Row], tableName: SingletonElem[String]) {
    def toData: Rep[TableIterData[Row]] = isoTableIter(eRow, tableName).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoTableIter[Row](implicit eRow: Elem[Row], tableName: SingletonElem[String]): Iso[TableIterData[Row], TableIter[Row]] =
    reifyObject(new TableIterIso[Row]()(eRow, tableName))

  // 6) smart constructor and deconstructor
  def mkTableIter[Row]()(implicit eRow: Elem[Row], tableName: SingletonElem[String]): Rep[TableIter[Row]]
  def unmkTableIter[Row](p: Rep[Iter[Row]]): Option[(Rep[Unit])]

  abstract class AbsIndexIter[Row]
      ()(implicit eRow: Elem[Row], indexName: SingletonElem[String])
    extends IndexIter[Row]() with Def[IndexIter[Row]] {
    lazy val selfType = element[IndexIter[Row]]
  }
  // elem for concrete class
  class IndexIterElem[Row](val iso: Iso[IndexIterData[Row], IndexIter[Row]])(implicit override val eRow: Elem[Row], val indexName: SingletonElem[String])
    extends IterElem[Row, IndexIter[Row]]
    with ConcreteElem[IndexIterData[Row], IndexIter[Row]] {
    override lazy val parent: Option[Elem[_]] = Some(iterElement(element[Row]))
    override lazy val typeArgs = TypeArgs("Row" -> eRow)

    override def convertIter(x: Rep[Iter[Row]]) = IndexIter()
    override def getDefaultRep = IndexIter()
    override lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIter[Row]]
    }
  }

  // state representation type
  type IndexIterData[Row] = Unit

  // 3) Iso for concrete class
  class IndexIterIso[Row](implicit eRow: Elem[Row], indexName: SingletonElem[String])
    extends EntityIso[IndexIterData[Row], IndexIter[Row]] with Def[IndexIterIso[Row]] {
    override def from(p: Rep[IndexIter[Row]]) =
      ()
    override def to(p: Rep[Unit]) = {
      val unit = p
      IndexIter()
    }
    lazy val eFrom = UnitElement
    lazy val eTo = new IndexIterElem[Row](self)
    lazy val selfType = new IndexIterIsoElem[Row](eRow, indexName)
    def productArity = 2
    def productElement(n: Int) = (eRow, indexName).productElement(n)
  }
  case class IndexIterIsoElem[Row](eRow: Elem[Row], indexName: SingletonElem[String]) extends Elem[IndexIterIso[Row]] {
    def isEntityType = true
    def getDefaultRep = reifyObject(new IndexIterIso[Row]()(eRow, indexName))
    lazy val tag = {
      implicit val tagRow = eRow.tag
      weakTypeTag[IndexIterIso[Row]]
    }
    lazy val typeArgs = TypeArgs("Row" -> eRow)
  }
  // 4) constructor and deconstructor
  class IndexIterCompanionAbs extends CompanionDef[IndexIterCompanionAbs] {
    def selfType = IndexIterCompanionElem
    override def toString = "IndexIter"
    @scalan.OverloadId("fromData")
    def apply[Row](p: Rep[IndexIterData[Row]])(implicit eRow: Elem[Row], indexName: SingletonElem[String]): Rep[IndexIter[Row]] =
      isoIndexIter(eRow, indexName).to(p)
    @scalan.OverloadId("fromFields")
    def apply[Row]()(implicit eRow: Elem[Row], indexName: SingletonElem[String]): Rep[IndexIter[Row]] =
      mkIndexIter()

    def unapply[Row](p: Rep[Iter[Row]]) = unmkIndexIter(p)
  }
  lazy val IndexIterRep: Rep[IndexIterCompanionAbs] = new IndexIterCompanionAbs
  lazy val IndexIter: IndexIterCompanionAbs = proxyIndexIterCompanion(IndexIterRep)
  implicit def proxyIndexIterCompanion(p: Rep[IndexIterCompanionAbs]): IndexIterCompanionAbs = {
    proxyOps[IndexIterCompanionAbs](p)
  }

  implicit case object IndexIterCompanionElem extends CompanionElem[IndexIterCompanionAbs] {
    lazy val tag = weakTypeTag[IndexIterCompanionAbs]
    protected def getDefaultRep = IndexIter
  }

  implicit def proxyIndexIter[Row](p: Rep[IndexIter[Row]]): IndexIter[Row] =
    proxyOps[IndexIter[Row]](p)

  implicit class ExtendedIndexIter[Row](p: Rep[IndexIter[Row]])(implicit eRow: Elem[Row], indexName: SingletonElem[String]) {
    def toData: Rep[IndexIterData[Row]] = isoIndexIter(eRow, indexName).from(p)
  }

  // 5) implicit resolution of Iso
  implicit def isoIndexIter[Row](implicit eRow: Elem[Row], indexName: SingletonElem[String]): Iso[IndexIterData[Row], IndexIter[Row]] =
    reifyObject(new IndexIterIso[Row]()(eRow, indexName))

  // 6) smart constructor and deconstructor
  def mkIndexIter[Row]()(implicit eRow: Elem[Row], indexName: SingletonElem[String]): Rep[IndexIter[Row]]
  def unmkIndexIter[Row](p: Rep[Iter[Row]]): Option[(Rep[Unit])]

  registerModule(Iters_Module)
}

// Std -----------------------------------
trait ItersStd extends ScalanStd with ItersDsl {
  self: ItersDsl with ScalanSqlStd =>

  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }

  case class StdTableIter[Row]
      ()(implicit eRow: Elem[Row], tableName: SingletonElem[String])
    extends AbsTableIter[Row]() {
  }

  def mkTableIter[Row]
    ()(implicit eRow: Elem[Row], tableName: SingletonElem[String]): Rep[TableIter[Row]] =
    new StdTableIter[Row]()
  def unmkTableIter[Row](p: Rep[Iter[Row]]) = p match {
    case p: TableIter[Row] @unchecked =>
      Some(())
    case _ => None
  }

  case class StdIndexIter[Row]
      ()(implicit eRow: Elem[Row], indexName: SingletonElem[String])
    extends AbsIndexIter[Row]() {
  }

  def mkIndexIter[Row]
    ()(implicit eRow: Elem[Row], indexName: SingletonElem[String]): Rep[IndexIter[Row]] =
    new StdIndexIter[Row]()
  def unmkIndexIter[Row](p: Rep[Iter[Row]]) = p match {
    case p: IndexIter[Row] @unchecked =>
      Some(())
    case _ => None
  }
}

// Exp -----------------------------------
trait ItersExp extends ScalanExp with ItersDsl {
  self: ItersDsl with ScalanSqlExp =>

  lazy val Iter: Rep[IterCompanionAbs] = new IterCompanionAbs {
  }

  case class ExpTableIter[Row]
      ()(implicit eRow: Elem[Row], tableName: SingletonElem[String])
    extends AbsTableIter[Row]()

  object TableIterMethods {
  }

  def mkTableIter[Row]
    ()(implicit eRow: Elem[Row], tableName: SingletonElem[String]): Rep[TableIter[Row]] =
    new ExpTableIter[Row]()
  def unmkTableIter[Row](p: Rep[Iter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: TableIterElem[Row] @unchecked =>
      Some(())
    case _ =>
      None
  }

  case class ExpIndexIter[Row]
      ()(implicit eRow: Elem[Row], indexName: SingletonElem[String])
    extends AbsIndexIter[Row]()

  object IndexIterMethods {
  }

  def mkIndexIter[Row]
    ()(implicit eRow: Elem[Row], indexName: SingletonElem[String]): Rep[IndexIter[Row]] =
    new ExpIndexIter[Row]()
  def unmkIndexIter[Row](p: Rep[Iter[Row]]) = p.elem.asInstanceOf[Elem[_]] match {
    case _: IndexIterElem[Row] @unchecked =>
      Some(())
    case _ =>
      None
  }

  object IterMethods {
    object map {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "map" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, eB, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapU" =>
          Some((receiver, f, eB)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Elem[B]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object flatMap {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "flatMap" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Iter[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object filter {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "filter" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object isEmpty {
      def unapply(d: Def[_]): Option[Rep[Iter[Row]] forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "isEmpty" =>
          Some(receiver).asInstanceOf[Option[Rep[Iter[Row]] forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Rep[Iter[Row]] forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduce {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "reduce" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => B], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object reduceU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = d match {
        case MethodCall(receiver, method, Seq(f, init, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "reduceU" =>
          Some((receiver, f, init)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((B, Row)) => Unit], Rep[Thunk[B]]) forSome {type Row; type B}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduce {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapReduce" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => V]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object mapReduceU {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = d match {
        case MethodCall(receiver, method, Seq(mapKey, packKey, newValue, reduceValue, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "mapReduceU" =>
          Some((receiver, mapKey, packKey, newValue, reduceValue)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => K], Rep[Row => String], Rep[Thunk[V]], Rep[((V, Row)) => Unit]) forSome {type Row; type K; type V}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sort {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "sort" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Boolean]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object sortBy {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(comparator, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "sortBy" =>
          Some((receiver, comparator)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[((Row, Row)) => Int]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    object join {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = d match {
        case MethodCall(receiver, method, Seq(other, thisKey, otherKey, cloneOther, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "join" =>
          Some((receiver, other, thisKey, otherKey, cloneOther)).asInstanceOf[Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], RIter[B], Rep[Row => Key], Rep[B => Key], Rep[B => B]) forSome {type Row; type B; type Key}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }

    // WARNING: Cannot generate matcher for method `toArray`: Method's return type Arr[Row] is not a Rep

    object materialize {
      def unapply(d: Def[_]): Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}] = d match {
        case MethodCall(receiver, method, Seq(f, _*), _) if receiver.elem.isInstanceOf[IterElem[_, _]] && method.getName == "materialize" =>
          Some((receiver, f)).asInstanceOf[Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[(Rep[Iter[Row]], Rep[Row => Row]) forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }

  object IterCompanionMethods {
    object empty {
      def unapply(d: Def[_]): Option[Unit forSome {type Row}] = d match {
        case MethodCall(receiver, method, _, _) if receiver.elem == IterCompanionElem && method.getName == "empty" =>
          Some(()).asInstanceOf[Option[Unit forSome {type Row}]]
        case _ => None
      }
      def unapply(exp: Exp[_]): Option[Unit forSome {type Row}] = exp match {
        case Def(d) => unapply(d)
        case _ => None
      }
    }
  }
}

object Iters_Module extends scalan.ModuleInfo {
  val dump = "H4sIAAAAAAAAALVWTYgcRRR+Mzuz87eJ6+pKAorrOlExOrMYJMIiYd2dyIbJ7LC9MTIGpaa7ZtKxurq3u2bt8RDBQw7qScSD4CGgeAmCeFMIARVERNCrZ08xEnIwJ8VX1T/TM25vftA5FN01r9/P933vVV26BnnPhcc8nTDCaxYVpKap5xVPVLUGF6YYnrSNAaNrtPfz4St2+e33WlmY7cD0WeKteawDpeCh4TvxsyaMJpQI16knbNcT8EhTRajrNmNUF6bN66ZlDQTpMlpvmp5YbkKuaxvDbTgPmSbM6jbXXSqotsqI51Ev3C9SmZEZv5fU+3DDGcXgdVlFPVHFlktMgeljjNnAfpM62pDbfGgJ2B+mtuHItNCmQn0Ha1i3HKbCTDWhYFqO7YooagEjnLWN6DXHCW7AXPMc2SF1jNqva8I1eV86c4j+OunTFppI8xzW4FHW2xo6NHRe8YQxFs93AABZeUYlVhthVosxq0nMqhp1TcLMN4n8s+3a/hCCX2YKwHfQxVO3cBF5oA1uVN85o79yU6tYWfmxL1MpqISm0dHDKQpR9CC232++79148eLRLJQ7UDa9la4nXKKLpAxCuCqEc1uonGMEidtHBhfTGFRRVtBmQiYl3bYcwtFTiOUMEsVM3RTSWO7NhPSkYF8QDo1MM76TietdSKlXaWmVMNa+evDpQ783Xs5CdjxECV1q2Axu5FRAbl1QN3Qt13sETG3abyiE5VLyR2thj+AxDI9f/cP4bgnOZGPwwlixS+mmEpDfsjmtHm9X/9R++OCS5NaFmeCfQPJ/m0f/+nV/TyjalaP526MdM5l77qOvDtH251kodlSXHGekrwQgQV6jnt6Bor1D3WC/sEOYfNpVBAWD9siAiZCaJKZTiKmAhdQGd6gEfBkJRO3HGBxE5CniHGGeazBq7UmD2n5w0klJ9Yvs4OizfRp2N6PC5gmXLjyaJhqHtl3TwsG1Q5/95utT1y+38ko3c2HJLxE2oMHMCCseVS8FlF1cFDA9MogEg7ltydykvlSycSoPpaeCtD2w2byfXTt2OQv5E5DvISNeE/Jde8CNqJFwAAvqixeivcw4I9g4xCVWpLtg7CyASkJhtzu2lcw4/nfWGUqbI4mPKp5PuPgXff+NBkxuUP9WGpDL4QmC1uWHkwTJdek2QEuktFc7RifcFxcuzF//5LX71BQudk1hEae6dAczOBqZ/+OMhQn0Me2710SKMhCqcjDiNNui9y7eMF+9+K5Q0zLjj5/UG91zeDIuKz8HlJ8WJBztJjlsyH0yx9UkMAGB0bC/e67lujH6vIOl1FJYX6M6Iy415M2BWnizCfg88uGx0ycOnD6lyp0xlFHwTzxCd7+HnSTOsro1PLHHrQGNqg3LwVshPhz59vlf3vrxs0/V0TFCSUBecShCFnjN22ZxOYsp5WihbFAQ529+3Hrypy9/U0OyLAWIZxiP71/J4TgulKKKi7epEXu4WwqiaNssATcyJYUapiVX5dL8B/EY1kQTCwAA"
}
}

