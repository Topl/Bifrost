package co.topl.codecs.bytes.tetra

import co.topl.codecs.bytes.scodecs.valuetypes.{intCodec, longCodec}
import co.topl.codecs.bytes.typeclasses.ImmutableCodec
import co.topl.models.BlockHeaderV2.Unsigned.PartialOperationalCertificate
import co.topl.models._
import co.topl.models.utility.Ratio

trait TetraImmutableCodecs {
  import TetraScodecCodecs._

  implicit val ratioStableCodec: ImmutableCodec[Ratio] =
    ImmutableCodec.fromScodecCodec

  implicit val taktikosAddressStableCodec: ImmutableCodec[TaktikosAddress] =
    ImmutableCodec.fromScodecCodec

  implicit val eligibilityCertificateStableCodec: ImmutableCodec[EligibilityCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val operationalCertificateStableCodec: ImmutableCodec[OperationalCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val partialOperationalCertificateStableCodec: ImmutableCodec[PartialOperationalCertificate] =
    ImmutableCodec.fromScodecCodec

  implicit val unsignedHeaderV2StableCodec: ImmutableCodec[BlockHeaderV2.Unsigned] =
    ImmutableCodec.fromScodecCodec

  implicit val transactionStableCodec: ImmutableCodec[Transaction] =
    ImmutableCodec.fromScodecCodec

  implicit val extendedEd25519VKStableCodec: ImmutableCodec[VerificationKeys.ExtendedEd25519] =
    ImmutableCodec.fromScodecCodec

  implicit val ed25519VRFVKStableCodec: ImmutableCodec[VerificationKeys.VrfEd25519] =
    ImmutableCodec.fromScodecCodec

  implicit val longStableCodec: ImmutableCodec[Long] =
    ImmutableCodec.fromScodecCodec

  implicit val intStableCodec: ImmutableCodec[Int] =
    ImmutableCodec.fromScodecCodec

}

object TetraImmutableCodecs extends TetraImmutableCodecs
