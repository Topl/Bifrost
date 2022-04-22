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

  implicit val showTaktikosAddress: Show[TaktikosAddress] =
    showBytes.contramap[TaktikosAddress](_.immutableBytes)

  import IdentityOps._

  implicit val showBlockHeaderV2: Show[BlockHeaderV2] =
    header =>
      show"BlockHeader(id=${header.id.asTypedBytes}" +
      show" parentId=${header.parentHeaderId}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" address=${header.address}"

  implicit val showInetSocketAddress: Show[InetSocketAddress] =
    address => s"${address.getHostName}:${address.getPort}"
}

object ShowInstances extends ShowInstances
