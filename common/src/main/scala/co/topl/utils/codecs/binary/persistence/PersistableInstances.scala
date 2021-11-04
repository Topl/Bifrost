package co.topl.utils.codecs.binary.persistence

import co.topl.attestation._
import co.topl.attestation.keyManagement._
import co.topl.crypto._
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.signatures.Signature
import co.topl.modifier.ModifierId
import co.topl.modifier.block._
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.codecs.binary.typeclasses.Persistable
import co.topl.utils.codecs.binary.scodecs._

trait PersistableInstances {

  implicit val digest32Persistable: Persistable[Digest32] = Persistable.fromCodec

  implicit val privateKeyCurve25519Persistable: Persistable[PrivateKeyCurve25519] = Persistable.fromCodec

  implicit val privateKeyEd25519Persistable: Persistable[PrivateKeyEd25519] = Persistable.fromCodec

  implicit val signatureCurve25519Persistable: Persistable[SignatureCurve25519] = Persistable.fromCodec

  implicit val signatureEd25519Persistable: Persistable[SignatureEd25519] = Persistable.fromCodec

  implicit val thresholdSignatureCurve25519Persistable: Persistable[ThresholdSignatureCurve25519] =
    Persistable.fromCodec

  implicit val proofPersistable: Persistable[Proof[_]] = Persistable.fromCodec

  implicit val proofPropositionCurve25519Persistable: Persistable[Proof[PublicKeyPropositionCurve25519]] =
    Persistable.fromCodec

  implicit val proofPropositionEd25519Persistable: Persistable[Proof[PublicKeyPropositionEd25519]] =
    Persistable.fromCodec

  implicit val proofPropositionThresholdCurve25519Persistable: Persistable[Proof[ThresholdPropositionCurve25519]] =
    Persistable.fromCodec

  implicit val publicKeyPropositionCurve25519Persistable: Persistable[PublicKeyPropositionCurve25519] =
    Persistable.fromCodec

  implicit val publicKeyPropositionEd25519Persistable: Persistable[PublicKeyPropositionEd25519] = Persistable.fromCodec

  implicit val thresholdPropositionCurve25519Persistable: Persistable[ThresholdPropositionCurve25519] =
    Persistable.fromCodec

  implicit val propositionPersistable: Persistable[Proposition] = Persistable.fromCodec

  implicit val evidencePersistable: Persistable[Evidence] = Persistable.fromCodec

  implicit val addressPersistable: Persistable[Address] = Persistable.fromCodec

  implicit val modifierIdPersistable: Persistable[ModifierId] = Persistable.fromCodec

  implicit val bloomFilterPersistable: Persistable[BloomFilter] = Persistable.fromCodec

  implicit val blockPersistable: Persistable[Block] = Persistable.fromCodec

  implicit val blockHeaderPersistable: Persistable[BlockHeader] = Persistable.fromCodec

  implicit val blockBodyPersistable: Persistable[BlockBody] = Persistable.fromCodec

  implicit val securityRootPersistable: Persistable[SecurityRoot] = Persistable.fromCodec

  implicit val assetCodePersistable: Persistable[AssetCode] = Persistable.fromCodec

  implicit val simpleValuePersistable: Persistable[SimpleValue] = Persistable.fromCodec

  implicit val assetValuePersistable: Persistable[AssetValue] = Persistable.fromCodec

  implicit val boxPersistable: Persistable[Box[_]] = Persistable.fromCodec

  implicit val arbitBoxPersistable: Persistable[ArbitBox] = Persistable.fromCodec

  implicit val boxIdPersistable: Persistable[BoxId] = Persistable.fromCodec

  implicit val polyTransferPersistable: Persistable[PolyTransfer[_ <: Proposition]] = Persistable.fromCodec

  implicit val arbitTransferPersistable: Persistable[ArbitTransfer[_ <: Proposition]] = Persistable.fromCodec

  implicit val assetTransferPersistable: Persistable[AssetTransfer[_ <: Proposition]] = Persistable.fromCodec

  implicit val transactionPersistable: Persistable[Transaction.TX] = Persistable.fromCodec

}
