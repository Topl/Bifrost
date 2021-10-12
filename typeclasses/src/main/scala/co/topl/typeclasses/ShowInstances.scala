package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.codecs.bytes.BasicCodecs._
import co.topl.crypto.Base58
import co.topl.models._
import co.topl.models.utility.{Length, Sized}
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import java.time.Instant

trait ShowInstances {

  implicit val showBytes: Show[Bytes] =
    bytes => Base58.encode(bytes.toArray)

  implicit def showSizedBytes[Data: Show, L <: Length](implicit l: L): Show[Sized.Strict[Data, L]] =
    sized => show"[${l.value}](${sized.data})"

  implicit val showTypedIdentifier: Show[TypedIdentifier] =
    showBytes.contramap[TypedIdentifier](_.allBytes)

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
