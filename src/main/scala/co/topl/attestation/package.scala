package co.topl

import co.topl.attestation.proof.{Proof, SignatureCurve25519, ThresholdSignatureCurve25519}
import co.topl.attestation.proposition.{Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519}
import io.circe.{ACursor, DecodingFailure}

package object attestation {
  def jsonDecoder(propType: String, attestation: ACursor): Either[DecodingFailure, Map[_ <: Proposition, _ <: Proof[_]]] = {
    propType match {
      case "PublicKeyCurve25519" => attestation.
      case "ThresholdCurve25519" => attestation.as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]]
    }
  }
}
