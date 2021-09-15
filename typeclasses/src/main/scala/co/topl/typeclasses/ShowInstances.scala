package co.topl.typeclasses

import cats.Show
import cats.implicits._
import co.topl.models._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import java.time.Instant

trait ShowInstances {

  implicit val showBytes: Show[Bytes] =
    bytes => Base58.encode(bytes.toArray)

  implicit val showTypedIdentifier: Show[TypedIdentifier] =
    showBytes.contramap[TypedIdentifier](_.allBytes)

  implicit val showBlockHeaderV2: Show[BlockHeaderV2] =
    header =>
      show"BlockHeader(id=${header.id}" +
      show" parentId=${header.parentHeaderId}" +
      show" height=${header.height}" +
      show" slot=${header.slot}" +
      show" timestamp=${Instant.ofEpochMilli(header.timestamp).toString})"
}

object ShowInstances extends ShowInstances
