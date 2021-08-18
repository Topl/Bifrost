package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

/**
 * Satisfies that T can be uniquely identified
 */
@typeclass trait Identifiable[T] {
  @op("id") def idOf(t: T): TypedIdentifier
}

object Identifiable {

  object Instances {

    implicit val identifiableBlock: Identifiable[Block] = {
      case b: BlockV1 => ???
      case b: BlockV2 => ???
    }

    implicit val transactionIdentifiable: Identifiable[Transaction] = ???
  }
}

object IdentifierTypes {

  object Block {
    val V1: TypePrefix = 1: Byte
    val V2: TypePrefix = 4: Byte
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
