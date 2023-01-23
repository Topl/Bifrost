package co.topl.codecs.bytes.tetra

import cats.data.Chain
import co.topl.codecs.bytes.tetra.TetraImmutableCodecs._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.{models => legacyModels}
import legacyModels._
import co.topl.consensus.models.BlockHeader
import co.topl.proto.models.Transaction
import scodec.bits.ByteVector

trait TetraIdentifiableInstances {

  implicit val identifiableBlockHeader: Identifiable[legacyModels.BlockHeader] =
    (header: legacyModels.BlockHeader) => {
      val bytes =
        header.parentHeaderId.allBytes ++
        header.txRoot.data ++
        header.bloomFilter.data ++
        Bytes(BigInt(header.timestamp).toByteArray) ++
        Bytes(BigInt(header.height).toByteArray) ++
        Bytes(BigInt(header.slot).toByteArray) ++
        header.eligibilityCertificate.immutableBytes ++
        header.operationalCertificate.immutableBytes ++
        Bytes(header.metadata.fold(Array.emptyByteArray)(_.data.bytes)) ++
        header.address.immutableBytes

      (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(bytes))
    }

  implicit val identifiableConsensusBlockHeader: Identifiable[BlockHeader] =
    (header: BlockHeader) => {
      val bytes =
        TypedBytes.headerFromBlockId(header.parentHeaderId).allBytes ++
        ByteVector(header.txRoot.toByteArray) ++
        ByteVector(header.bloomFilter.toByteArray) ++ Bytes(
          BigInt(header.timestamp).toByteArray
        ) ++
        Bytes(BigInt(header.height).toByteArray) ++
        Bytes(BigInt(header.slot).toByteArray) ++
        header.eligibilityCertificate.map(_.immutableBytes).getOrElse(Bytes.empty) ++
        header.operationalCertificate.map(_.immutableBytes).getOrElse(Bytes.empty) ++
        Bytes(header.metadata.toByteArray) ++
        header.address.immutableBytes
      (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(bytes))
    }

  implicit val transactionIdentifiable: Identifiable[legacyModels.Transaction] =
    transaction => {
      val bytes =
        legacyModels.Transaction
          .Unproven(
            transaction.inputs.map(i => legacyModels.Transaction.Unproven.Input(i.boxId, i.proposition, i.value)),
            transaction.outputs,
            transaction.schedule,
            transaction.data
          )
          .immutableBytes
      val hash = new Blake2b256().hash(bytes)
      (IdentifierTypes.Transaction, hash)
    }

  implicit val transactionProtoIdentifiable: Identifiable[Transaction] =
    transaction => {
      val bytes =
        legacyModels.Transaction
          .UnprovenProto(
            inputs = Chain.fromSeq(
              transaction.inputs.map(i => legacyModels.Transaction.Unproven.InputProto(i.boxId, i.proposition, i.value))
            ),
            outputs = Chain.fromSeq(transaction.outputs),
            transaction.schedule,
            transaction.data
          )
          .immutableBytes
      val hash = new Blake2b256().hash(bytes)
      (IdentifierTypes.Transaction, hash)
    }
}

object TetraIdentifiableInstances extends TetraIdentifiableInstances
