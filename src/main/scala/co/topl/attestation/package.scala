package co.topl

import co.topl.attestation.proof.{ Proof, SignatureCurve25519, ThresholdSignatureCurve25519 }
import co.topl.attestation.proposition.{ Proposition, PublicKeyPropositionCurve25519, ThresholdPropositionCurve25519 }
import io.circe.syntax.EncoderOps
import io.circe.{ ACursor, DecodingFailure, Json }

package object attestation {
  def jsonDecoder[P <: Proposition] (propType: String, attestation: ACursor): Either[DecodingFailure, Map[P, _ <: Proof[P]]] = {
    (propType match {
      case "PublicKeyCurve25519" => attestation.as[Map[PublicKeyPropositionCurve25519, SignatureCurve25519]]
      case "ThresholdCurve25519" => attestation.as[Map[ThresholdPropositionCurve25519, ThresholdSignatureCurve25519]]
    }).map {
        case att: Map[P, _ <: Proof[P]] => att
      }
    }

  def jsonEncoder[P <: Proposition](att: Map[P, _ <: Proof[P]]): Json =
    att.map {
      case (prop, proof) => prop.toString -> proof.toString
    }.asJson
}
