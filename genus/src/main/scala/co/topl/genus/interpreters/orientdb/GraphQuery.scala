package co.topl.genus.interpreters.orientdb

import cats._
import cats.implicits._

sealed trait GraphQuery[Result] {
  def stringify: (String, Array[Any])
}

case class Raw[Result](query: String, args: Array[Any]) extends GraphQuery[Result] {
  def stringify: (String, Array[Any]) = (query, args)
}

case class NodesByClass[Result: NodeSchema](where: Where = Where.Any) extends GraphQuery[Result] {

  def stringify: (String, Array[Any]) = {
    import Where.WhereOps
    val wts = where.queryString
    val query =
      wts.foldLeft(s"SELECT FROM ${NodeSchema[Result].name}") { case (q, (w, _)) =>
        s"$q WHERE $w"
      }

    (query, wts.fold(Array.empty[Any])(_._2))
  }

}

/**
 * A non-recursive query which follows edges from an origin node
 */
case class Trace[Origin: NodeSchema, Result](where: Where, edges: List[EdgeWithDirection]) extends GraphQuery[Result] {

  def in[E: EdgeSchema[*, Res, Result], Res: NodeSchema]: Trace[Origin, Res] =
    Trace[Origin, Res](where = where, edges = edges :+ EdgeWithDirection(implicitly[EdgeSchema[E, Res, Result]], In))

  def out[E: EdgeSchema[*, Result, Res], Res: NodeSchema]: Trace[Origin, Res] =
    Trace[Origin, Res](where = where, edges = edges :+ EdgeWithDirection(implicitly[EdgeSchema[E, Result, Res]], Out))

  override def stringify: (String, Array[Any]) = {
    val expansion = edges
      .map { edgeWithDirection =>
        val directionString =
          edgeWithDirection.direction match {
            case In  => "in"
            case Out => "out"
          }
        s"$directionString('${edgeWithDirection.edgeSchema.name}')"
      }
      .mkString(".")
    val wts = where.queryString
    val query =
      s"SELECT expand($expansion)" +
      s" FROM ${implicitly[NodeSchema[Origin]].name}" +
      wts.map(_._1).fold("")(" WHERE " + _)
    (query, wts.fold(Array.empty[Any])(_._2))
  }
}

object Trace {

  def apply[Origin](where: Where = Where.Any)(implicit originNodeSchema: NodeSchema[Origin]): Trace[Origin, Origin] =
    Trace[Origin, Origin](where, Nil)
}

case class EdgeWithDirection(edgeSchema: EdgeSchema[_, _, _], direction: Direction)
sealed abstract class Direction
case object In extends Direction
case object Out extends Direction

sealed abstract class Where

object Where {
  case object Any extends Where
  case class PropEquals(name: String, value: Any) extends Where
  case class And(op1: Where, op2: Where) extends Where
  case class Or(op1: Where, op2: Where) extends Where

  implicit class WhereOps(where: Where) {

    private val andSemigroup: Semigroup[(String, Array[Any])] =
      (a, b) => (s"${a._1} && ${b._1}", a._2 ++ b._2)

    private val orSemigroup: Semigroup[(String, Array[Any])] =
      (a, b) => (s"${a._1} || ${b._1}", a._2 ++ b._2)

    def queryString: Option[(String, Array[Any])] =
      where match {
        case Any                     => None
        case PropEquals(name, value) => (name + "=?", Array(value)).some
        case And(op1, op2) =>
          implicit def semigroup: Semigroup[(String, Array[Any])] = andSemigroup
          op1.queryString.combine(op2.queryString)
        case Or(op1, op2) =>
          implicit def semigroup: Semigroup[(String, Array[Any])] = orSemigroup
          op1.queryString.combine(op2.queryString)
      }
  }
}
