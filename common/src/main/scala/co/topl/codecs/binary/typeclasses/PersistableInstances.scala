package co.topl.codecs.binary.typeclasses

import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.attestation._
import co.topl.crypto.hash.digest.Digest32
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.codecs.binary._
import co.topl.utils.Int128

trait PersistableInstances {

  implicit val digest32Persistable: Persistable[Digest32] = Persistable.instanceFromCodec

  implicit val privateKeyCurve25519Persistable: Persistable[PrivateKeyCurve25519] =
    Persistable.instanceFromCodec

  implicit val privateKeyEd25519Persistable: Persistable[PrivateKeyEd25519] =
    Persistable.instanceFromCodec

  implicit val signatureCurve25519Persistable: Persistable[SignatureCurve25519] = Persistable.instanceFromCodec

  implicit val signatureEd25519Persistable: Persistable[SignatureEd25519] = Persistable.instanceFromCodec

  implicit val thresholdSignatureCurve25519Persistable: Persistable[ThresholdSignatureCurve25519] =
    Persistable.instanceFromCodec

  implicit val proofPersistable: Persistable[Proof[_ <: Proposition]] = Persistable.instanceFromCodec

  implicit val proofPropositionCurve25519Persistable: Persistable[Proof[PublicKeyPropositionCurve25519]] =
    Persistable.instanceFromCodec

  implicit val proofPropositionEd25519Persistable: Persistable[Proof[PublicKeyPropositionEd25519]] =
    Persistable.instanceFromCodec

  implicit val proofPropositionThresholdCurve25519Persistable: Persistable[Proof[ThresholdPropositionCurve25519]] =
    Persistable.instanceFromCodec

  implicit val publicKeyPropositionCurve25519Persistable: Persistable[PublicKeyPropositionCurve25519] =
    Persistable.instanceFromCodec

  implicit val publicKeyPropositionEd25519Persistable: Persistable[PublicKeyPropositionEd25519] =
    Persistable.instanceFromCodec

  implicit val thresholdPropositionCurve25519Persistable: Persistable[ThresholdPropositionCurve25519] =
    Persistable.instanceFromCodec

  implicit val propositionPersistable: Persistable[Proposition] = Persistable.instanceFromCodec

  implicit val evidencePersistable: Persistable[Evidence] = Persistable.instanceFromCodec

  implicit val addressPersistable: Persistable[Address] = Persistable.instanceFromCodec

  implicit val modifierIdPersistable: Persistable[ModifierId] = Persistable.instanceFromCodec

  implicit val bloomFilterPersistable: Persistable[BloomFilter] = Persistable.instanceFromCodec

  implicit val blockPersistable: Persistable[Block] = Persistable.instanceFromCodec

  implicit val blockHeaderPersistable: Persistable[BlockHeader] = Persistable.instanceFromCodec

  implicit val blockBodyPersistable: Persistable[BlockBody] = Persistable.instanceFromCodec

  implicit val securityRootPersistable: Persistable[SecurityRoot] = Persistable.instanceFromCodec

  implicit val assetCodePersistable: Persistable[AssetCode] = Persistable.instanceFromCodec

  implicit val simpleValuePersistable: Persistable[SimpleValue] = Persistable.instanceFromCodec

  implicit val assetValuePersistable: Persistable[AssetValue] = Persistable.instanceFromCodec

  implicit val boxPersistable: Persistable[Box[_]] = Persistable.instanceFromCodec

  implicit val arbitBoxPersistable: Persistable[ArbitBox] = Persistable.instanceFromCodec

  implicit val boxIdPersistable: Persistable[BoxId] = Persistable.instanceFromCodec

  implicit val polyTransferPersistable: Persistable[PolyTransfer[_ <: Proposition]] = Persistable.instanceFromCodec

  implicit val arbitTransferPersistable: Persistable[ArbitTransfer[_ <: Proposition]] = Persistable.instanceFromCodec

  implicit val assetTransferPersistable: Persistable[AssetTransfer[_ <: Proposition]] = Persistable.instanceFromCodec

  implicit val transactionPersistable: Persistable[Transaction.TX] = Persistable.instanceFromCodec

  implicit val nodeViewModifierPersistable: Persistable[NodeViewModifier] = Persistable.instanceFromCodec

  implicit val longPersistable: Persistable[Long] = Persistable.instanceFromCodec[Long](longCodec)

  implicit val intPersistable: Persistable[Int] = Persistable.instanceFromCodec[Int](intCodec)

  implicit val int128Persistable: Persistable[Int128] = Persistable.instanceFromCodec

}
