package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.codecs.bytes.implicits._
import co.topl.models._
import co.topl.models.utility.{Base58, Length, Sized}
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import java.time.Instant

trait ShowInstances {

  // todo: this should use Base58 in Utils?
  implicit val showBytes: Show[Bytes] =
    bytes => Base58.encode(bytes.toArray)

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showTypedIdentifier: Show[TypedIdentifier] =
    showBytes.contramap[TypedIdentifier](_.allBytes)

  implicit val showSlotId: Show[SlotId] =
    slotID => show"{${slotID.slot},${slotID.blockId}}"

  implicit val showTaktikosAddress: Show[TaktikosAddress] =
    showBytes.contramap[TaktikosAddress](_.bytes)

  implicit val showBlockHeaderV2: Show[BlockHeaderV2] =
    header =>
      show"BlockHeader(id=${header.id}" +
      show" parentId=${header.parentHeaderId}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})" +
      show" address=${header.address.bytes}"
}

object ShowInstances extends ShowInstances
