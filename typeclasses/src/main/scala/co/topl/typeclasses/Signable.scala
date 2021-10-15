package co.topl.typeclasses

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.models.{BlockHeaderV2, Bytes, VerificationKeys}
import simulacrum.{op, typeclass}

@typeclass trait Signable[T] {

  /**
   * Turns some value into an array of bytes that can be used in a signing routine
   * @param t A value that can be represented as bytes
   */
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
        unsignedBlock.eligibilityCertificate.bytes ++
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
            header.eligibilityCertificate,
            header.metadata,
            header.address
          )
        )

    implicit val byteArraySignable: Signable[Array[Byte]] = Bytes(_)

    implicit val vkVrfSignable: Signable[VerificationKeys.VrfEd25519] =
      _.bytes.data
  }
  object instances extends Instances
}
