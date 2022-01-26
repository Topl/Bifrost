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

  implicit val showBytes: Show[Array[Byte]] = fromBase58

  implicit val addressShow: Show[Address] = fromBase58

  implicit def digestShow[T: Digest]: Show[T] = fromBase58

  implicit val evidenceShow: Show[Evidence] = fromBase58

  implicit val signatureCurve25519Show: Show[SignatureCurve25519] = fromBase58

  implicit val thresholdSignatureCurve25519Show: Show[ThresholdSignatureCurve25519] = fromBase58

  implicit val signatureEd25519Show: Show[SignatureEd25519] = fromBase58

  implicit val proofShow: Show[Proof[_ <: Proposition]] = fromBase58

  implicit val publicKeyPropositionCurve25519Show: Show[PublicKeyPropositionCurve25519] = fromBase58

  implicit val thresholdPropositionCurve25519Show: Show[ThresholdPropositionCurve25519] = fromBase58

  implicit val publicKeyPropositionEd25519Show: Show[PublicKeyPropositionEd25519] = fromBase58

  implicit val propositionShow: Show[Proposition] = fromBase58

  implicit val privateKeyCurve25519Show: Show[PrivateKeyCurve25519] = fromBase58

  implicit val privateKeyEd25519Show: Show[PrivateKeyEd25519] = fromBase58

  implicit val arbitTransferShow: Show[ArbitTransfer[_ <: Proposition]] = arbitTransfer =>
    arbitTransferJsonEncoder(arbitTransfer).spaces2

  implicit val assetTransferShow: Show[AssetTransfer[_ <: Proposition]] = assetTransfer =>
    assetTransferJsonEncoder(assetTransfer).spaces2

  implicit val polyTransferShow: Show[PolyTransfer[_ <: Proposition]] = polyTransfer =>
    polyTransferJsonEncoder(polyTransfer).spaces2

  implicit val txShow: Show[Transaction.TX] = fromJsonEncoder

  implicit val securityRootShow: Show[SecurityRoot] = fromBase58

  implicit val boxIdShow: Show[BoxId] = fromBase58

  implicit val assetCodeShow: Show[AssetCode] = fromBase58

  implicit val bloomFilterShow: Show[BloomFilter] = fromBase58

  implicit val arbitBoxShow: Show[ArbitBox] = fromJsonEncoder

  implicit val assetBoxShow: Show[AssetBox] = fromJsonEncoder

  implicit val polyBoxShow: Show[PolyBox] = fromJsonEncoder

  implicit val tokenValueHolderShow: Show[TokenValueHolder] = fromJsonEncoder

  implicit val assetValueShow: Show[AssetValue] = fromJsonEncoder

  implicit val simpleValueShow: Show[SimpleValue] = fromJsonEncoder

  implicit val blockShow: Show[Block] = fromJsonEncoder

  implicit val blockHeaderShow: Show[BlockHeader] = fromJsonEncoder

  implicit val blockBodyShow: Show[BlockBody] = fromJsonEncoder

  implicit val modifierIdShow: Show[ModifierId] = fromBase58

  implicit val boxShow: Show[Box[_]] = fromJsonEncoder

  implicit val programIdShow: Show[ProgramId] = fromBase58

  implicit val codeBoxShow: Show[CodeBox] = fromJsonEncoder

  implicit val stateBoxShow: Show[StateBox] = fromJsonEncoder

  implicit val executionBoxShow: Show[ExecutionBox] = fromJsonEncoder
}
