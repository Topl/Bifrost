package co.topl.modifier.transaction.unsigned

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519, ThresholdPropositionCurve25519}
import co.topl.utils.Identifier

sealed trait PropositionType

object PropositionTypes {
  case object PublicKeyCurve25519 extends PropositionType
  case object PublicKeyEd25519 extends PropositionType
  case object ThresholdCurve25519 extends PropositionType
}

object PropositionType {

  def getTypePrefix(prop: PropositionType): Byte =
    prop match {
      case PropositionTypes.PublicKeyCurve25519 => 1.toByte
      case PropositionTypes.PublicKeyEd25519    => 1.toByte
      case PropositionTypes.ThresholdCurve25519 => 1.toByte
    }

  def fromPropositionId(id: Identifier): Option[PropositionType] =
    id match {
      case x if x == Identifier(PublicKeyPropositionCurve25519.typeString, PublicKeyPropositionCurve25519.typePrefix) =>
        PropositionTypes.PublicKeyCurve25519.some
      case x if x == Identifier(PublicKeyPropositionEd25519.typeString, PublicKeyPropositionCurve25519.typePrefix) =>
        PropositionTypes.PublicKeyEd25519.some
      case x if x == Identifier(ThresholdPropositionCurve25519.typeString, ThresholdPropositionCurve25519.typePrefix) =>
        PropositionTypes.ThresholdCurve25519.some
      case _ => None
    }

  def fromPropositionTypeString(typeString: String): Option[PropositionType] =
    typeString match {
      case PublicKeyPropositionCurve25519.`typeString` =>
        PropositionTypes.PublicKeyCurve25519.some
      case ThresholdPropositionCurve25519.`typeString` =>
        PropositionTypes.ThresholdCurve25519.some
      case PublicKeyPropositionEd25519.`typeString` =>
        PropositionTypes.PublicKeyEd25519.some
      case _ => None
    }
}
