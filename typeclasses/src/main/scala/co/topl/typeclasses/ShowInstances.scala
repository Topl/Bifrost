package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Identifiable
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.models._
import co.topl.models.utility.{Length, Sized}

import java.net.InetSocketAddress
import java.time.Instant

trait ShowInstances {

  implicit def showInstanceFromIdentifiable[T: Identifiable]: Show[T] =
    t => {
      val (prefix, bytes) = t.id
      show"($prefix)$bytes"
    }

  implicit val showBytes: Show[Bytes] =
    bytes => bytes.toBase58

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showTypedIdentifier: Show[TypedIdentifier] =
    showBytes.contramap[TypedIdentifier](_.allBytes)

  implicit val showSlotId: Show[SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showRho: Show[Rho] =
    _.sizedBytes.show

  implicit val showStakingAddressesOperator: Show[StakingAddresses.Operator] =
    showBytes.contramap[StakingAddresses.Operator](_.immutableBytes)

  implicit val showStakingAddress: Show[StakingAddress] =
    showBytes.contramap[StakingAddress](_.immutableBytes)

  import IdentityOps._

  implicit val showBlockHeader: Show[BlockHeader] =
    header =>
      show"BlockHeader(id=${header.id.asTypedBytes}" +
      show" parentId=${header.parentHeaderId}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" address=${header.address: StakingAddress}"

  implicit val showConsensusBlockHeader: Show[co.topl.consensus.models.BlockHeader] =
    header =>
      show"BlockHeader(id=${header.id.asTypedBytes}" +
      show" parentId=${TypedBytes.headerFromBlockId(header.parentHeaderId)}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" address=${co.topl.models.StakingAddresses.operatorFromProtoString(header.address).show}"

  implicit val showNodeBlockBody: Show[co.topl.node.models.BlockBody] =
    body => show"${body.transactionIds.map(TypedBytes.ioTx32)}"

  implicit val showInetSocketAddress: Show[InetSocketAddress] =
    address => s"${address.getHostName}:${address.getPort}"

  implicit val showBoxId: Show[Box.Id] =
    boxId => show"${boxId.transactionId}.outputs[${boxId.transactionOutputIndex}]"
}

object ShowInstances extends ShowInstances
