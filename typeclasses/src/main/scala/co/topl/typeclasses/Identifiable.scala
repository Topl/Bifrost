package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

import java.nio.charset.StandardCharsets

/**
 * Satisfies that T can be uniquely identified
 */
@typeclass trait Identifiable[T] {
  @op("id") def idOf(t: T): TypedIdentifier
  def typePrefix: TypePrefix
}

object Identifiable {

  trait Instances {

    implicit val identifiableBlockHeaderV2: Identifiable[BlockHeaderV2] =
      new Identifiable[BlockHeaderV2] {

        override def idOf(t: BlockHeaderV2): TypedIdentifier =
          TypedBytes(IdentifierTypes.Block.HeaderV2 +: Bytes(s"header${t.height}".getBytes(StandardCharsets.UTF_8)))

        override def typePrefix: TypePrefix = IdentifierTypes.Block.HeaderV2
      }

    // TODO: hash(blockHeaderId ++ txRoot)
    implicit val identifiableBlockBodyV2: Identifiable[BlockBodyV2] =
      new Identifiable[BlockBodyV2] {

        override def idOf(t: BlockBodyV2): TypedIdentifier =
          TypedBytes(
            IdentifierTypes.Block.BodyV2 +: Bytes(s"bodyOf${t.headerId}".getBytes(StandardCharsets.UTF_8))
          )

        override def typePrefix: TypePrefix = IdentifierTypes.Block.BodyV2
      }

    implicit val identifiableBlockV1: Identifiable[BlockV1] =
      new Identifiable[BlockV1] {
        override def idOf(t: BlockV1): TypedIdentifier = ???

        override def typePrefix: TypePrefix = ???
      }

    implicit val transactionIdentifiable: Identifiable[Transaction] =
      new Identifiable[Transaction] {
        override def idOf(t: Transaction): TypedIdentifier = ???

        override def typePrefix: TypePrefix = ???
      }
  }
  object Instances extends Instances
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

  val RatioEvidence = 6: Byte
}
