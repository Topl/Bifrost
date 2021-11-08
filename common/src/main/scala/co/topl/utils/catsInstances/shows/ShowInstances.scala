package co.topl.utils.catsInstances.shows

import cats.Show
import co.topl.attestation._
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.codecs._
import co.topl.crypto.hash.digest.Digest
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}

trait ShowInstances {

  implicit val showBytes: Show[Array[Byte]] = fromBinaryShow

  implicit val addressShow: Show[Address] = fromBinaryShow

  implicit def digestShow[T: Digest]: Show[T] = fromBinaryShow

  implicit val evidenceShow: Show[Evidence] = fromBinaryShow

  implicit val signatureCurve25519Show: Show[SignatureCurve25519] = fromBinaryShow

  implicit val thresholdSignatureCurve25519Show: Show[ThresholdSignatureCurve25519] = fromBinaryShow

  implicit val signatureEd25519Show: Show[SignatureEd25519] = fromBinaryShow

  implicit val proofShow: Show[Proof[_ <: Proposition]] = fromBinaryShow

  implicit val publicKeyPropositionCurve25519Show: Show[PublicKeyPropositionCurve25519] = fromBinaryShow

  implicit val thresholdPropositionCurve25519Show: Show[ThresholdPropositionCurve25519] = fromBinaryShow

  implicit val publicKeyPropositionEd25519Show: Show[PublicKeyPropositionEd25519] = fromBinaryShow

  implicit val propositionShow: Show[Proposition] = fromBinaryShow

  implicit val privateKeyCurve25519Show: Show[PrivateKeyCurve25519] = fromBinaryShow

  implicit val privateKeyEd25519Show: Show[PrivateKeyEd25519] = fromBinaryShow

  implicit def arbitTransferShow[P <: Proposition]: Show[ArbitTransfer[P]] = fromJsonEncoder(arbitTransferJsonEncoder)

  implicit def assetTransferShow[P <: Proposition]: Show[AssetTransfer[P]] = fromJsonEncoder(assetTransferJsonEncoder)

  implicit def polyTransferShow[P <: Proposition]: Show[PolyTransfer[P]] = fromJsonEncoder(polyTransferJsonEncoder)

  implicit val txShow: Show[Transaction.TX] = fromJsonEncoder

  implicit val securityRootShow: Show[SecurityRoot] = fromBinaryShow

  implicit val boxIdShow: Show[BoxId] = fromBinaryShow

  implicit val assetCodeShow: Show[AssetCode] = fromBinaryShow

  implicit val bloomFilterShow: Show[BloomFilter] = fromBinaryShow

  implicit val arbitBoxShow: Show[ArbitBox] = fromJsonEncoder

  implicit val assetBoxShow: Show[AssetBox] = fromJsonEncoder

  implicit val polyBoxShow: Show[PolyBox] = fromJsonEncoder

  implicit val tokenValueHolderShow: Show[TokenValueHolder] = fromJsonEncoder

  implicit val assetValueShow: Show[AssetValue] = fromJsonEncoder

  implicit val simpleValueShow: Show[SimpleValue] = fromJsonEncoder

  implicit val blockShow: Show[Block] = fromJsonEncoder

  implicit val blockHeaderShow: Show[BlockHeader] = fromJsonEncoder

  implicit val blockBodyShow: Show[BlockBody] = fromJsonEncoder

  implicit val modifierIdShow: Show[ModifierId] = fromBinaryShow

  implicit val boxShow: Show[Box[_]] = fromJsonEncoder
}
