package co.topl.codecs.bytes.tetra

import cats.data.Chain
import co.topl.codecs.bytes.tetra.TetraImmutableCodecs._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader}
import scodec.bits.ByteVector

trait TetraIdentifiableInstances {

  implicit val identifiableBlockHeader: Identifiable[BlockHeader] =
    (header: BlockHeader) => {
      val bytes =
        header.parentHeaderId.allBytes ++ header.txRoot.data ++ header.bloomFilter.data ++ Bytes(
          BigInt(header.timestamp).toByteArray
        ) ++
        Bytes(BigInt(header.height).toByteArray) ++
        Bytes(BigInt(header.slot).toByteArray) ++
        header.eligibilityCertificate.immutableBytes ++
        header.operationalCertificate.immutableBytes ++
        Bytes(header.metadata.fold(Array.emptyByteArray)(_.data.bytes)) ++
        header.address.immutableBytes

      (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(bytes))
    }

  implicit val identifiableConsensusBlockHeader: Identifiable[ConsensusBlockHeader] =
    (header: ConsensusBlockHeader) => {
      val bytes =
        ByteVector(header.parentHeaderId.toByteArray) ++
        ByteVector(header.txRoot.toByteArray) ++
        ByteVector(header.bloomFilter.toByteArray) ++ Bytes(
          BigInt(header.timestamp).toByteArray
        ) ++
        Bytes(BigInt(header.height).toByteArray) ++
        Bytes(BigInt(header.slot).toByteArray)
//      ++ TODO, : Ask Sean, should we create the representation of Inmutablebyte of all protobubspcsModels?
//          header.eligibilityCertificate.immutableBytes ++
//          header.operationalCertificate.immutableBytes ++
//          Bytes(header.metadata.fold(Array.emptyByteArray)(_.data.bytes)) ++
//          header.address.immutableBytes
      (IdentifierTypes.Block.HeaderV2, new Blake2b256().hash(bytes))
    }

  implicit val transactionIdentifiable: Identifiable[Transaction] =
    transaction => {
      val bytes =
        Transaction
          .Unproven(
            transaction.inputs.map(i => Transaction.Unproven.Input(i.boxId, i.proposition, i.value)),
            transaction.outputs,
            transaction.schedule,
            transaction.data
          )
          .immutableBytes
      val hash = new Blake2b256().hash(bytes)
      (IdentifierTypes.Transaction, hash)
    }

  implicit val transactionProtoIdentifiable: Identifiable[co.topl.proto.models.Transaction] =
    transaction => {
      val bytes =
        Transaction
          .UnprovenProto(
            inputs = Chain.fromSeq(
              transaction.inputs.map(i => Transaction.Unproven.InputProto(i.boxId, i.proposition, i.value))
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
