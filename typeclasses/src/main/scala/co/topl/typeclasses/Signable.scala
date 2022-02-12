package co.topl.typeclasses

import co.topl.codecs.bytes.implicits._
import co.topl.models.{BlockHeaderV2, Bytes, Transaction, VerificationKeys}
import co.topl.typeclasses.TransactionOps.instances._
import com.google.common.primitives.{Ints, Longs}
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
        Bytes.concat(
          List(
            unsignedBlock.parentHeaderId.allBytes,
            unsignedBlock.txRoot.data,
            unsignedBlock.bloomFilter.data,
            Bytes(BigInt(unsignedBlock.timestamp).toByteArray),
            Bytes(BigInt(unsignedBlock.height).toByteArray),
            Bytes(BigInt(unsignedBlock.slot).toByteArray),
            unsignedBlock.eligibilityCertificate.bytes,
            unsignedBlock.partialOperationalCertificate.bytes,
            Bytes(unsignedBlock.metadata.fold(Array.emptyByteArray)(_.data.bytes)),
            unsignedBlock.address.bytes
          )
        )

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
            BlockHeaderV2.Unsigned.PartialOperationalCertificate(
              header.operationalCertificate.parentVK,
              header.operationalCertificate.parentSignature,
              header.operationalCertificate.childVK
            ),
            header.metadata,
            header.address
          )
        )

    implicit val byteArraySignable: Signable[Array[Byte]] = Bytes(_)
    implicit val bytesSignable: Signable[Bytes] = identity

    implicit val vkVrfSignable: Signable[VerificationKeys.VrfEd25519] =
      _.bytes.data

    implicit val transactionSignable: Signable[Transaction] = t =>
      unprovenTransactionSignable.signableBytesOf(t.unproven)

    implicit val unprovenTransactionSignable: Signable[Transaction.Unproven] = t =>
      Bytes.concat(
        List(Bytes(Ints.toByteArray(t.inputs.length))) ++
        t.inputs.map { case (a, r) => a.allBytes ++ Bytes(Longs.toByteArray(r)) } ++
        t.feeOutput.map(o => o.dionAddress.allBytes ++ Bytes(o.value.data.toByteArray)) ++
        List(Bytes(Ints.toByteArray(t.coinOutputs.length.toInt)))
        // TODO: Rest of the data
      )
  }
  object instances extends Instances
}
