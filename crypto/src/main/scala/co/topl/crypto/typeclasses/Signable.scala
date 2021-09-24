package co.topl.crypto.typeclasses

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.models.{BlockHeaderV2, Bytes}
import simulacrum.{op, typeclass}

@typeclass trait Signable[T] {
  @op("signableBytes") def signableBytesOf(t: T): Bytes
}

object Signable {

  trait Instances {

    implicit val unsignedBlockHeaderV2Signable: Signable[BlockHeaderV2.Unsigned] =
      unsignedBlock =>
        unsignedBlock.parentHeaderId.allBytes ++ unsignedBlock.txRoot.data ++ unsignedBlock.bloomFilter.data ++ Bytes(
          BigInt(unsignedBlock.timestamp).toByteArray
        ) ++
        Bytes(BigInt(unsignedBlock.height).toByteArray) ++
        Bytes(BigInt(unsignedBlock.slot).toByteArray) ++
        unsignedBlock.eligibibilityCertificate.bytes ++
        Bytes(unsignedBlock.metadata.fold(Array.emptyByteArray)(_.data.value)) ++
        unsignedBlock.address.bytes

    implicit val blockHeaderV2Signable: Signable[BlockHeaderV2] =
      header =>
        unsignedBlockHeaderV2Signable.signableBytesOf(
          BlockHeaderV2.Unsigned(
            header.parentHeaderId,
            header.parentSlot,
            header.txRoot,
            header.bloomFilter,
            header.timestamp,
            header.height,
            header.slot,
            header.eligibibilityCertificate,
            header.metadata,
            header.address
          )
        )

    implicit val byteArray: Signable[Array[Byte]] = Bytes(_)
  }
  object instances extends Instances
}
