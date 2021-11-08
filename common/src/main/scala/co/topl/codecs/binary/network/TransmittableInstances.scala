package co.topl.codecs.binary.network

import co.topl.attestation._
import co.topl.attestation.keyManagement._
import co.topl.crypto._
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.signatures.Signature
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.modifier.block._
import co.topl.modifier.box._
import co.topl.modifier.transaction._
import co.topl.codecs.binary.typeclasses.Transmittable
import co.topl.codecs.binary.scodecs._

trait TransmittableInstances {

  implicit val digest32Transmittable: Transmittable[Digest32] = Transmittable.fromCodec

  implicit val privateKeyCurve25519Transmittable: Transmittable[PrivateKeyCurve25519] = Transmittable.fromCodec

  implicit val privateKeyEd25519Transmittable: Transmittable[PrivateKeyEd25519] = Transmittable.fromCodec

  implicit val signatureCurve25519Transmittable: Transmittable[SignatureCurve25519] = Transmittable.fromCodec

  implicit val signatureEd25519Transmittable: Transmittable[SignatureEd25519] = Transmittable.fromCodec

  implicit val thresholdSignatureCurve25519Transmittable: Transmittable[ThresholdSignatureCurve25519] =
    Transmittable.fromCodec

  implicit def proofTransmittable: Transmittable[Proof[_ <: Proposition]] = Transmittable.fromCodec

  implicit val proofPropositionCurve25519Transmittable: Transmittable[Proof[PublicKeyPropositionCurve25519]] =
    Transmittable.fromCodec

  implicit val proofPropositionEd25519Transmittable: Transmittable[Proof[PublicKeyPropositionEd25519]] =
    Transmittable.fromCodec

  implicit val proofPropositionThresholdCurve25519Transmittable: Transmittable[Proof[ThresholdPropositionCurve25519]] =
    Transmittable.fromCodec

  implicit val publicKeyPropositionCurve25519Transmittable: Transmittable[PublicKeyPropositionCurve25519] =
    Transmittable.fromCodec

  implicit val publicKeyPropositionEd25519Transmittable: Transmittable[PublicKeyPropositionEd25519] =
    Transmittable.fromCodec

  implicit val thresholdPropositionCurve25519Transmittable: Transmittable[ThresholdPropositionCurve25519] =
    Transmittable.fromCodec

  implicit val propositionTransmittable: Transmittable[Proposition] = Transmittable.fromCodec

  implicit val evidenceTransmittable: Transmittable[Evidence] = Transmittable.fromCodec

  implicit val addressTransmittable: Transmittable[Address] = Transmittable.fromCodec

  implicit val modifierIdTransmittable: Transmittable[ModifierId] = Transmittable.fromCodec

  implicit val bloomFilterTransmittable: Transmittable[BloomFilter] = Transmittable.fromCodec

  implicit val blockTransmittable: Transmittable[Block] = Transmittable.fromCodec

  implicit val blockHeaderTransmittable: Transmittable[BlockHeader] = Transmittable.fromCodec

  implicit val blockBodyTransmittable: Transmittable[BlockBody] = Transmittable.fromCodec

  implicit val securityRootTransmittable: Transmittable[SecurityRoot] = Transmittable.fromCodec

  implicit val assetCodeTransmittable: Transmittable[AssetCode] = Transmittable.fromCodec

  implicit val simpleValueTransmittable: Transmittable[SimpleValue] = Transmittable.fromCodec

  implicit val assetValueTransmittable: Transmittable[AssetValue] = Transmittable.fromCodec

  implicit val boxTransmittable: Transmittable[Box[_]] = Transmittable.fromCodec

  implicit val boxIdTransmittable: Transmittable[BoxId] = Transmittable.fromCodec

  implicit val polyTransferTransmittable: Transmittable[PolyTransfer[_ <: Proposition]] = Transmittable.fromCodec

  implicit val arbitTransferTransmittable: Transmittable[ArbitTransfer[_ <: Proposition]] = Transmittable.fromCodec

  implicit val assetTransferTransmittable: Transmittable[AssetTransfer[_ <: Proposition]] = Transmittable.fromCodec

  implicit val transactionTransmittable: Transmittable[Transaction.TX] = Transmittable.fromCodec

  implicit val nodeViewModifierTransmittable: Transmittable[NodeViewModifier] = Transmittable.fromCodec

}
