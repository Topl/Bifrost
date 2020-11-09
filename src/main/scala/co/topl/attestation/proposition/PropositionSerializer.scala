package co.topl.attestation.proposition

import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object PropositionSerializer extends BifrostSerializer[Proposition] {
  override def serialize(obj: Proposition, w: Writer): Unit = {
    obj match {
      case obj: PublicKeyCurve25519Proposition =>
        w.put(PublicKeyCurve25519Proposition.typePrefix)
        PublicKeyCurve25519PropositionSerializer.serialize(obj, w)

      case obj: ThresholdCurve25519Proposition =>
        w.put(ThresholdCurve25519Proposition.typePrefix)
        ThresholdCurve25519PropositionSerializer.serialize(obj, w)
    }
  }

  override def parse(r: Reader): Proposition = {
    r.getByte() match {
      case PublicKeyCurve25519Proposition.typePrefix => PublicKeyCurve25519PropositionSerializer.parse(r)
      case ThresholdCurve25519Proposition.typePrefix => ThresholdCurve25519PropositionSerializer.parse(r)
    }
  }
}
