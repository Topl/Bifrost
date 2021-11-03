package co.topl.typeclasses

import co.topl.models._
import simulacrum.{op, typeclass}

import java.nio.charset.StandardCharsets
import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.crypto.hash.blake2b256

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

        override def idOf(header: BlockHeaderV2): TypedIdentifier = {
          val bytes =
            header.parentHeaderId.allBytes ++ header.txRoot.data ++ header.bloomFilter.data ++ Bytes(
              BigInt(header.timestamp).toByteArray
            ) ++
            Bytes(BigInt(header.height).toByteArray) ++
            Bytes(BigInt(header.slot).toByteArray) ++
            header.eligibilityCertificate.bytes ++
            //header.operationalCertificate.bytes ++
            Bytes(header.metadata.fold(Array.emptyByteArray)(_.data.value)) ++
            header.address.bytes

          TypedBytes(IdentifierTypes.Block.HeaderV2 +: Bytes(blake2b256.hash(bytes.toArray).value))
        }

        override def typePrefix: TypePrefix = IdentifierTypes.Block.HeaderV2
      }

    implicit val identifiableBlockBodyV2: Identifiable[BlockBodyV2] =
      new Identifiable[BlockBodyV2] {

        override def idOf(t: BlockBodyV2): TypedIdentifier =
//          import ContainsTransactions.ops._
//          import ContainsTransactions.Instances._
//          val bytes = t.headerId.allBytes ++ t.transactions.merkleTree.data
//          TypedBytes(IdentifierTypes.Block.BodyV2 +: Bytes(blake2b256.hash(bytes.toArray).value))
          t.headerId

        override def typePrefix: TypePrefix = IdentifierTypes.Block.HeaderV2
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
