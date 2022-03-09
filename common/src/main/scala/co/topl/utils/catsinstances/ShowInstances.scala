package co.topl.utils.catsinstances

import cats.Show
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.attestation._
import co.topl.codecs.binary.typeclasses.BinaryShow
import co.topl.codecs._
import co.topl.crypto.hash.digest.Digest
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.builder.BuildTransferFailure
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.StringDataTypes.{
  Base16Data,
  Base58Data,
  DataEncodingValidationFailure,
  DataEncodingValidationFailures,
  Latin1Data
}
import co.topl.utils.encode.{Base16, Base58}
import io.circe.Encoder

import java.nio.charset.StandardCharsets

trait ShowInstances {

  implicit val showBytes: Show[Array[Byte]] = asBase58

  implicit val base58Show: Show[Base58Data] = asBase58

  implicit val base16Show: Show[Base16Data] = asBase16

  implicit val dataEncodingValidationFailureShow: Show[DataEncodingValidationFailure] = {
    case DataEncodingValidationFailures.InvalidCharacter(character) =>
      s"'$character' is an invalid character"
    case DataEncodingValidationFailures.NonEvenLength => s"data must be an even length"
  }

  implicit val latin1DataShow: Show[Latin1Data] = data => new String(data.value, StandardCharsets.ISO_8859_1)

  implicit val addressShow: Show[Address] = asBase58

  implicit def digestShow[T: Digest]: Show[T] = asBase58

  implicit val evidenceShow: Show[Evidence] = asBase58

  implicit val signatureCurve25519Show: Show[SignatureCurve25519] = asBase58

  implicit val thresholdSignatureCurve25519Show: Show[ThresholdSignatureCurve25519] = asBase58

  implicit val signatureEd25519Show: Show[SignatureEd25519] = asBase58

  implicit val proofShow: Show[Proof[_ <: Proposition]] = asBase58

  implicit val publicKeyPropositionCurve25519Show: Show[PublicKeyPropositionCurve25519] = asBase58

  implicit val thresholdPropositionCurve25519Show: Show[ThresholdPropositionCurve25519] = asBase58

  implicit val publicKeyPropositionEd25519Show: Show[PublicKeyPropositionEd25519] = asBase58

  implicit val propositionShow: Show[Proposition] = asBase58

  implicit val privateKeyCurve25519Show: Show[PrivateKeyCurve25519] = asBase58

  implicit val privateKeyEd25519Show: Show[PrivateKeyEd25519] = asBase58

  implicit val arbitTransferShow: Show[ArbitTransfer[_ <: Proposition]] = arbitTransfer =>
    arbitTransferJsonEncoder(arbitTransfer).spaces2

  implicit val assetTransferShow: Show[AssetTransfer[_ <: Proposition]] = assetTransfer =>
    assetTransferJsonEncoder(assetTransfer).spaces2

  implicit val polyTransferShow: Show[PolyTransfer[_ <: Proposition]] = polyTransfer =>
    polyTransferJsonEncoder(polyTransfer).spaces2

  implicit val txShow: Show[Transaction.TX] = asJsonWithSpaces

  implicit val securityRootShow: Show[SecurityRoot] = asBase58

  implicit val boxIdShow: Show[BoxId] = asBase58

  implicit val assetCodeShow: Show[AssetCode] = asBase58

  implicit val bloomFilterShow: Show[BloomFilter] = asBase58

  implicit val arbitBoxShow: Show[ArbitBox] = asJsonWithSpaces

  implicit val assetBoxShow: Show[AssetBox] = asJsonWithSpaces

  implicit val polyBoxShow: Show[PolyBox] = asJsonWithSpaces

  implicit val tokenValueHolderShow: Show[TokenValueHolder] = asJsonWithSpaces

  implicit val assetValueShow: Show[AssetValue] = asJsonWithSpaces

  implicit val simpleValueShow: Show[SimpleValue] = asJsonWithSpaces

  implicit val blockShow: Show[Block] = asJsonWithSpaces

  implicit val blockHeaderShow: Show[BlockHeader] = asJsonWithSpaces

  implicit val blockBodyShow: Show[BlockBody] = asJsonWithSpaces

  implicit val modifierIdShow: Show[ModifierId] = asBase58

  implicit val boxShow: Show[Box[_]] = asJsonWithSpaces

//  implicit val programIdShow: Show[ProgramId] = asBase58
//
//  implicit val codeBoxShow: Show[CodeBox] = asJsonWithSpaces
//
//  implicit val stateBoxShow: Show[StateBox] = asJsonWithSpaces
//
//  implicit val executionBoxShow: Show[ExecutionBox] = asJsonWithSpaces

  private def asBase58[T: BinaryShow]: Show[T] = value => Base58.encode(BinaryShow[T].encodeAsBytes(value))

  private def asBase16[T: BinaryShow]: Show[T] = value => Base16.encode(BinaryShow[T].encodeAsBytes(value))

  private def asJsonWithSpaces[T: Encoder]: Show[T] = value => Encoder[T].apply(value).spaces2

  implicit val buildTransferFailureShow: Show[BuildTransferFailure] = Show.fromToString
}
