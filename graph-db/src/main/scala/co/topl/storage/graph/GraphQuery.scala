package co.topl.storage.graph

sealed abstract class GraphQuery[Result](implicit val resultSchema: NodeSchema[Result])

case class Raw[Result: NodeSchema](query: String) extends GraphQuery[Result]

case class NodesByClass[Result: NodeSchema](where: Where = WhereAny) extends GraphQuery[Result]

case class Trace[Origin, Result](where: Where, edges: List[EdgeWithDirection])(implicit
  val originNodeSchema:                 NodeSchema[Origin],
  resultNodeSchema:                     NodeSchema[Result]
) extends GraphQuery[Result] {

  def in[E, Res](edgeSchema: EdgeSchema[E, Res, Result]): Trace[Origin, Res] =
    Trace[Origin, Res](where = where, edges = edges :+ EdgeWithDirection(edgeSchema, In))(
      originNodeSchema = originNodeSchema,
      resultNodeSchema = edgeSchema.srcSchema
    )

  def out[E, Res](edgeSchema: EdgeSchema[E, Result, Res]): Trace[Origin, Res] =
    Trace[Origin, Res](where = where, edges = edges :+ EdgeWithDirection(edgeSchema, Out))(
      originNodeSchema = originNodeSchema,
      resultNodeSchema = edgeSchema.destSchema
    )
}

object Trace {

  def apply[Origin](where: Where = WhereAny)(implicit originNodeSchema: NodeSchema[Origin]): Trace[Origin, Origin] =
    Trace[Origin, Origin](where, Nil)
}

case class Traverse[Origin, Result](origin: NodesByClass[Origin], edges: List[EdgeWithDirection])(implicit
  val originNodeSchema:                     NodeSchema[Origin],
  resultNodeSchema:                         NodeSchema[Result]
) extends GraphQuery[Result] {

  def in[E, Res](edgeSchema: EdgeSchema[E, Res, Result]): Traverse[Origin, Res] =
    Traverse[Origin, Res](origin = origin, edges = edges :+ EdgeWithDirection(edgeSchema, In))(
      originNodeSchema = originNodeSchema,
      resultNodeSchema = edgeSchema.srcSchema
    )

  def out[E, Res](edgeSchema: EdgeSchema[E, Result, Res]): Traverse[Origin, Res] =
    Traverse[Origin, Res](origin = origin, edges = edges :+ EdgeWithDirection(edgeSchema, Out))(
      originNodeSchema = originNodeSchema,
      resultNodeSchema = edgeSchema.destSchema
    )
}

object Traverse {

  def apply[Origin](origin: NodesByClass[Origin])(implicit
    originNodeSchema:       NodeSchema[Origin]
  ): Traverse[Origin, Origin] =
    Traverse[Origin, Origin](origin, Nil)
}

case class EdgeWithDirection(edgeSchema: EdgeSchema[_, _, _], direction: Direction)
sealed abstract class Direction
case object In extends Direction
case object Out extends Direction

sealed trait Where

case object WhereAny extends Where

case class PropEquals(name: String, value: Any) extends Where

case class And(op1: Where, op2: Where) extends Where

case class Or(op1: Where, op2: Where) extends Where
