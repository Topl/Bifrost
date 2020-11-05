package co.topl.attestation.proposition

import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.secrets.Secret
import co.topl.utils.serialization.{ BifrostSerializer, BytesSerializable }
import com.google.common.primitives.Ints
import io.circe.{ Decoder, Encoder, HCursor, KeyEncoder }
import scorex.util.encode.Base58

import scala.util.{ Failure, Success, Try }

// Propositions are challenges that must be satisfied by the prover.
// In most cases, propositions are used by transactions issuers (spenders) to prove the right
// to use a UTXO in a transaction.
sealed trait Proposition extends BytesSerializable {

  def address(implicit networkPrefix: NetworkPrefix): Address

  override type M = Proposition
  override def serializer: BifrostSerializer[Proposition] = PropositionSerializer

  override def toString: String = Base58.encode(bytes)

  override def equals (obj: Any): Boolean = obj match {
    case prop: Proposition => prop.bytes sameElements bytes
    case _                 => false
  }

  override def hashCode(): Int = Ints.fromByteArray(bytes)

}

object Proposition {
  def fromString[P <: Proposition] (str: String): Try[P] =
    Base58.decode(str).flatMap(bytes => PropositionSerializer.parseBytes(bytes) match {
      case Success(prop: P) => Success(prop)
      case _                => Failure(new Error("Failed to parse a proposition from the given string"))
    })

  implicit def jsonEncoder[P <: Proposition]: Encoder[P] = {
    case prop: PublicKeyCurve25519Proposition => PublicKeyCurve25519Proposition.jsonEncoder(prop)
    case prop: ThresholdCurve25519Proposition => ThresholdCurve25519Proposition.jsonEncoder(prop)
  }

  implicit def jsonKeyEncoder[P <: Proposition]: KeyEncoder[P] = {
    case prop: PublicKeyCurve25519Proposition => PublicKeyCurve25519Proposition.jsonKeyEncoder(prop)
    case prop: ThresholdCurve25519Proposition => ThresholdCurve25519Proposition.jsonKeyEncoder(prop)
  }

  implicit def jsonDecoder[P <: Proposition]: Decoder[P] = { c: HCursor =>
    c.downField("type").as[String].map {
      case "CodeCreation"           => CodeCreation.jsonDecoder(c)
      case "ProgramCreation"        => ProgramCreation.jsonDecoder(c)
      case "ProgramMethodExecution" => ProgramMethodExecution.jsonDecoder(c)
      case "ProgramTransfer"        => ProgramTransfer.jsonDecoder(c)
      case "PolyTransfer"           => PolyTransfer.jsonDecoder(c)
      case "ArbitTransfer"          => ArbitTransfer.jsonDecoder(c)
      case "AssetTransfer"          => AssetTransfer.jsonDecoder(c)
      case "AssetCreation"          => AssetCreation.jsonDecoder(c)
      case "Coinbase"               => Coinbase.jsonDecoder(c)
    } match {
      case Right(tx) => tx
      case Left(ex)  => throw ex
    }
  }
}

// Knowledge propositions require the prover to supply a proof attesting to their knowledge
// of secret information.
trait KnowledgeProposition[S <: Secret] extends Proposition

