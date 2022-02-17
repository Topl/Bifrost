package co.topl.codecs.binary.legacy.attestation

import co.topl.attestation.{
  Proposition,
  PublicKeyPropositionCurve25519,
  PublicKeyPropositionEd25519,
  ThresholdPropositionCurve25519
}
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object PropositionSerializer extends BifrostSerializer[Proposition] {

  override def serialize(obj: Proposition, w: Writer): Unit =
    obj match {
      case obj: PublicKeyPropositionCurve25519 =>
        w.put(PublicKeyPropositionCurve25519.typePrefix)
        PublicKeyPropositionCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdPropositionCurve25519 =>
        w.put(ThresholdPropositionCurve25519.typePrefix)
        ThresholdPropositionCurve25519Serializer.serialize(obj, w)

      case obj: PublicKeyPropositionEd25519 =>
        w.put(PublicKeyPropositionEd25519.typePrefix)
        PublicKeyPropositionEd25519Serializer.serialize(obj, w)
    }

  override def parse(r: Reader): Proposition =
    r.getByte() match {
      case PublicKeyPropositionCurve25519.`typePrefix` => PublicKeyPropositionCurve25519Serializer.parse(r)
      case ThresholdPropositionCurve25519.`typePrefix` => ThresholdPropositionCurve25519Serializer.parse(r)
      case PublicKeyPropositionEd25519.`typePrefix`    => PublicKeyPropositionEd25519Serializer.parse(r)
    }
}
