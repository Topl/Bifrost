package co.topl.codecs.binary.typeclasses

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.attestation._
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.codecs.binary.scodecs._

trait TransmittableInstances {

  implicit val digest32Transmittable: Transmittable[Digest32] = Transmittable.instanceFromCodec

  implicit val privateKeyCurve25519Transmittable: Transmittable[PrivateKeyCurve25519] = Transmittable.instanceFromCodec

  implicit val privateKeyEd25519Transmittable: Transmittable[PrivateKeyEd25519] = Transmittable.instanceFromCodec

  implicit val signatureCurve25519Transmittable: Transmittable[SignatureCurve25519] = Transmittable.instanceFromCodec

  implicit val signatureEd25519Transmittable: Transmittable[SignatureEd25519] = Transmittable.instanceFromCodec

  implicit val thresholdSignatureCurve25519Transmittable: Transmittable[ThresholdSignatureCurve25519] =
    Transmittable.instanceFromCodec

  implicit def proofTransmittable: Transmittable[Proof[_ <: Proposition]] = Transmittable.instanceFromCodec

  implicit val proofPropositionCurve25519Transmittable: Transmittable[Proof[PublicKeyPropositionCurve25519]] =
    Transmittable.instanceFromCodec

  implicit val proofPropositionEd25519Transmittable: Transmittable[Proof[PublicKeyPropositionEd25519]] =
    Transmittable.instanceFromCodec

  implicit val proofPropositionThresholdCurve25519Transmittable: Transmittable[Proof[ThresholdPropositionCurve25519]] =
    Transmittable.instanceFromCodec

  implicit val publicKeyPropositionCurve25519Transmittable: Transmittable[PublicKeyPropositionCurve25519] =
    Transmittable.instanceFromCodec

  implicit val publicKeyPropositionEd25519Transmittable: Transmittable[PublicKeyPropositionEd25519] =
    Transmittable.instanceFromCodec

  implicit val thresholdPropositionCurve25519Transmittable: Transmittable[ThresholdPropositionCurve25519] =
    Transmittable.instanceFromCodec

  implicit val propositionTransmittable: Transmittable[Proposition] = Transmittable.instanceFromCodec

  implicit val evidenceTransmittable: Transmittable[Evidence] = Transmittable.instanceFromCodec

  implicit val addressTransmittable: Transmittable[Address] = Transmittable.instanceFromCodec

  implicit val modifierIdTransmittable: Transmittable[ModifierId] = Transmittable.instanceFromCodec

  implicit val bloomFilterTransmittable: Transmittable[BloomFilter] = Transmittable.instanceFromCodec

  implicit val blockTransmittable: Transmittable[Block] = Transmittable.instanceFromCodec

  implicit val blockHeaderTransmittable: Transmittable[BlockHeader] = Transmittable.instanceFromCodec

  implicit val blockBodyTransmittable: Transmittable[BlockBody] = Transmittable.instanceFromCodec

  implicit val securityRootTransmittable: Transmittable[SecurityRoot] = Transmittable.instanceFromCodec

  implicit val assetCodeTransmittable: Transmittable[AssetCode] = Transmittable.instanceFromCodec

  implicit val simpleValueTransmittable: Transmittable[SimpleValue] = Transmittable.instanceFromCodec

  implicit val assetValueTransmittable: Transmittable[AssetValue] = Transmittable.instanceFromCodec

  implicit val boxTransmittable: Transmittable[Box[_]] = Transmittable.instanceFromCodec

  implicit val boxIdTransmittable: Transmittable[BoxId] = Transmittable.instanceFromCodec

  implicit val polyTransferTransmittable: Transmittable[PolyTransfer[_ <: Proposition]] =
    Transmittable.instanceFromCodec

  implicit val arbitTransferTransmittable: Transmittable[ArbitTransfer[_ <: Proposition]] =
    Transmittable.instanceFromCodec

  implicit val assetTransferTransmittable: Transmittable[AssetTransfer[_ <: Proposition]] =
    Transmittable.instanceFromCodec

  implicit val transactionTransmittable: Transmittable[Transaction.TX] = Transmittable.instanceFromCodec

  implicit val nodeViewModifierTransmittable: Transmittable[NodeViewModifier] = Transmittable.instanceFromCodec

}
