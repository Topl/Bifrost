package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T can be uniquely identified
 */
@typeclass trait Identifiable[T] {
  @op("id") def idOf(t: T): TypedIdentifier
  def typePrefix: TypePrefix
}

object Identifiable {

  object Instances {

    implicit val identifiableBlockHeaderV2: Identifiable[BlockHeaderV2] = ???
    implicit val identifiableBlockBodyV2: Identifiable[BlockBodyV2] = ???
    implicit val identifiableBlockV1: Identifiable[BlockV1] = ???

    implicit val transactionIdentifiable: Identifiable[Transaction] = ???
  }
}

object IdentifierTypes {

  object Block {
    val V1: TypePrefix = 1: Byte
    val HeaderV2: TypePrefix = 4: Byte
    val BodyV2: TypePrefix = 4: Byte
  }

  val Transaction: TypePrefix = 3: Byte

  object Box {
    val Arbit: TypePrefix = 1: Byte
    val Poly: TypePrefix = 2: Byte
    val Asset: TypePrefix = 3: Byte
    val Registration: TypePrefix = 4: Byte
    val Taktikos: TypePrefix = 5: Byte
  }
}
