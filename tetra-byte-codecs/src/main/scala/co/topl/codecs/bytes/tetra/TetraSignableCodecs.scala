package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.typeclasses._
import co.topl.models.{BlockHeaderV2, Bytes, Transaction}
import com.google.common.primitives.{Ints, Longs}

trait TetraSignableCodecs {

  import TetraStableCodecs._

  implicit val signableUnsignedBlockHeaderV2: Signable[BlockHeaderV2.Unsigned] =
    t => unsignedHeaderV2StableCodec.immutableBytes(t)

  implicit val signableBlockHeaderV2: Signable[BlockHeaderV2] =
    t =>
      signableUnsignedBlockHeaderV2.signableBytes(
        BlockHeaderV2.Unsigned(
          t.parentHeaderId,
          t.parentSlot,
          t.txRoot,
          t.bloomFilter,
          t.timestamp,
          t.height,
          t.slot,
          t.eligibilityCertificate,
          BlockHeaderV2.Unsigned.PartialOperationalCertificate(
            t.operationalCertificate.parentVK,
            t.operationalCertificate.parentSignature,
            t.operationalCertificate.childVK
          ),
          t.metadata,
          t.address
        )
      )

  // TODO
  implicit val signableTransaction: Signable[Transaction] =
    t =>
      Bytes.concat(
        List(Bytes(Ints.toByteArray(t.inputs.size))) ++
        t.inputs.map { case ((a, r), (prop, proof)) => a.allBytes ++ Bytes(Longs.toByteArray(r)) } ++
        t.feeOutput.map(o => o.dionAddress.allBytes ++ Bytes(o.value.data.toByteArray)) ++
        List(Bytes(Ints.toByteArray(t.coinOutputs.length.toInt)))
      )

  // TODO
  implicit val signableUnprovenTransaction: Signable[Transaction.Unproven] =
    t =>
      Bytes.concat(
        List(Bytes(Ints.toByteArray(t.inputs.length))) ++
        t.inputs.map { case (a, r) => a.allBytes ++ Bytes(Longs.toByteArray(r)) } ++
        t.feeOutput.map(o => o.dionAddress.allBytes ++ Bytes(o.value.data.toByteArray)) ++
        List(Bytes(Ints.toByteArray(t.coinOutputs.length.toInt)))
      )
}

object TetraSignableCodecs extends TetraSignableCodecs
