package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.SlotId
import co.topl.consensus.models.StakingAddress
import co.topl.models._
import co.topl.models.utility._
import co.topl.node.models.BlockBody
import com.google.protobuf.ByteString

import java.time.Instant

trait ShowInstances {

  implicit val showByteString: Show[ByteString] =
    bytes => bytes.toBase58

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showIoTransactionId: Show[TransactionId] =
    t => show"t_${t.value: Bytes}"

  implicit val showBlockId: Show[co.topl.consensus.models.BlockId] =
    b => show"b_${b.value: Bytes}"

  implicit val showConsensusSlotId: Show[SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showRho: Show[Rho] =
    _.sizedBytes.data.show

  implicit val showStakingAddress: Show[StakingAddress] =
    _.value.show

  implicit val showBlockHeader: Show[BlockHeader] =
    header =>
      show"BlockHeader(" +
      show"id=${header.id}" +
      show" parentId=${header.parentHeaderId}" +
      show" parentSlot=${header.parentSlot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      // Don't show these fields because they create too much noise in the logs
      // show" txRoot=${header.txRoot}" +
      // show" bloomFilter=${header.bloomFilter}" +
      // show" eligibilityCertificate=${header.eligibilityCertificate.toByteString}" +
      // show" operationalCertificate=${header.operationalCertificate.toByteString}" +
      show" address=${header.address}" +
      show")"

  implicit val showNodeBlockBody: Show[BlockBody] =
    body => show"${body.allTransactionIds}"

  implicit val showBoxId: Show[TransactionOutputAddress] =
    boxId => show"${boxId.id}.outputs[${boxId.index}]"
}

object ShowInstances extends ShowInstances
