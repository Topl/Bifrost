package attestation.serialization

import attestation.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import utils.serialization.{GjalSerializer, Reader, Writer}

/**
 * For serializing a proposition
 */
object PropositionSerializer extends GjalSerializer[Proposition] {

  override def serialize(obj: Proposition, w: Writer): Unit =
    obj match {
      case obj: PublicKeyPropositionCurve25519 =>
        w.put(PublicKeyPropositionCurve25519.typePrefix)
        PublicKeyPropositionCurve25519Serializer.serialize(obj, w)

      case obj: ThresholdPropositionCurve25519 =>
        w.put(ThresholdPropositionCurve25519.typePrefix)
        ThresholdPropositionCurve25519Serializer.serialize(obj, w)
    }

  override def parse(r: Reader): Proposition =
    r.getByte() match {
      case PublicKeyPropositionCurve25519.typePrefix => PublicKeyPropositionCurve25519Serializer.parse(r)
      case ThresholdPropositionCurve25519.typePrefix => ThresholdPropositionCurve25519Serializer.parse(r)
    }
}
