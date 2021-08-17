package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

@typeclass trait Identifiable[T] {
  @op("id") def id(t: T): TypedIdentifier
}

object IdentifiableInstances {

  implicit val identifiableBlock: Identifiable[Block] = {
    case b: BlockV1 => ???
    case b: BlockV2 => ???
  }

  implicit val transactionIdentifiable: Identifiable[Transaction] = ???

}

object IdentifierTypes {

  object Block {
    val V1 = 1: Byte
    val V2 = 4: Byte
  }

  val Transaction = 3: Byte

  object Box {
    val Arbit = 1: Byte
    val Poly = 2: Byte
    val Asset = 3: Byte
    val Registration = 4: Byte
    val Taktikos = 5: Byte
  }
}
