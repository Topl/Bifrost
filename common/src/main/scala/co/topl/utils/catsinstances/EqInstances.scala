package co.topl.utils.catsinstances

import cats.Eq
import cats.implicits._
import co.topl.attestation.keyManagement.{PrivateKeyCurve25519, PrivateKeyEd25519}
import co.topl.attestation._
import co.topl.codecs._
import co.topl.crypto.PublicKey
import co.topl.crypto.Signature
import co.topl.modifier.{ModifierId, ProgramId}
import co.topl.modifier.block.{Block, BlockBody, BlockHeader, BloomFilter}
import co.topl.modifier.box._
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data, Latin1Data}
import co.topl.crypto.implicits._
import co.topl.typeclasses.implicits._

import scala.collection.immutable.ListMap

trait EqInstances {

  implicit val bytesEq: Eq[Array[Byte]] = (b1, b2) => b1 sameElements b2

  implicit def seqEq[T: Eq]: Eq[Seq[T]] = (s1, s2) => s1.toList === s2.toList

  implicit val latin1DataEq: Eq[Latin1Data] = (l1, l2) => l1.value sameElements l2.value

  implicit val base16Eq: Eq[Base16Data] = (a, b) => a.encodeAsBytes sameElements b.encodeAsBytes

  implicit val base58Eq: Eq[Base58Data] = (a, b) => a.encodeAsBytes sameElements b.encodeAsBytes

  implicit val longsEq: Eq[Array[Long]] = (l1, l2) => l1 sameElements l2

  implicit val evidenceEq: Eq[Evidence] = (e1, e2) => e1.evBytes === e2.evBytes

  implicit val addressEq: Eq[Address] = (a1, a2) => a1.evidence === a2.evidence && a1.networkPrefix === a2.networkPrefix

  implicit val signatureEq: Eq[Signature] = (s1, s2) => s1.value === s2.value

  implicit val signatureCurve25519Eq: Eq[SignatureCurve25519] = (s1, s2) => s1.sigBytes === s2.sigBytes

  implicit val signatureEd25519Eq: Eq[SignatureEd25519] = (s1, s2) => s1.sigBytes === s2.sigBytes

  implicit val thresholdSignatureCurve25519Eq: Eq[ThresholdSignatureCurve25519] = (s1, s2) =>
    s1.signatures === s2.signatures

  implicit val proofEq: Eq[Proof[_ <: Proposition]] = (p1, p2) =>
    proofBinaryShow.encodeAsBytes(p1) === proofBinaryShow.encodeAsBytes(p2)

  implicit val publicKeyEq: Eq[PublicKey] = (p1, p2) => p1.value === p2.value

  implicit val publicKeyPropositionCurve25519Eq: Eq[PublicKeyPropositionCurve25519] = (p1, p2) =>
    p1.pubKeyBytes === p2.pubKeyBytes

  implicit val thresholdPropositionCurve25519Eq: Eq[ThresholdPropositionCurve25519] = (t1, t2) =>
    t1.threshold === t2.threshold && t1.pubKeyProps.toSet === t2.pubKeyProps.toSet

  implicit val publicKeyPropositionEd25519Eq: Eq[PublicKeyPropositionEd25519] = (p1, p2) =>
    p1.pubKeyBytes === p2.pubKeyBytes

  implicit def propositionEq[P <: Proposition]: Eq[P] = (p1, p2) =>
    propositionBinaryShow.encodeAsBytes(p1) === propositionBinaryShow.encodeAsBytes(p2)

  implicit val privateKeyCurve25519Eq: Eq[PrivateKeyCurve25519] = (p1, p2) =>
    p1.privateKey === p2.privateKey && p1.publicKey === p2.publicKey

  implicit val privateKeyEd25519Eq: Eq[PrivateKeyEd25519] = (p1, p2) => p1.privateKey === p2.privateKey

  implicit val boxNonceEq: Eq[Box.Nonce] = (b1, b2) => b1 === b2

  implicit val int128Eq: Eq[Int128] = (i1, i2) => i1.lowerLong === i2.lowerLong && i2.upperLong === i2.upperLong

  implicit val simpleValueEq: Eq[SimpleValue] = (s1, s2) => s1.quantity === s2.quantity

  implicit val attestationEq: Eq[ListMap[_ <: Proposition, Proof[_ <: Proposition]]] = (l1, l2) =>
    l1.size === l2.size &&
    l1.toList
      .zip(l2.toList)
      .forall(pair =>
        propositionBinaryShow.encodeAsBytes(pair._1._1) === propositionBinaryShow.encodeAsBytes(
          pair._2._1
        ) && proofBinaryShow.encodeAsBytes(pair._1._2) === proofBinaryShow.encodeAsBytes(pair._2._2)
      )

  implicit val arbitTransferEq: Eq[ArbitTransfer[_ <: Proposition]] = (a1, a2) =>
    a1.from.toSet === a2.from.toSet &&
    a1.to.toSet === a2.to.toSet &&
    attestationEq.eqv(a1.attestation, a2.attestation) &&
    a1.fee === a2.fee &&
    a1.timestamp === a2.timestamp &&
    a1.data === a2.data &&
    a1.minting === a2.minting

  implicit def polyTransferEq: Eq[PolyTransfer[_ <: Proposition]] = (p1, p2) =>
    p1.from.toSet === p2.from.toSet &&
    p1.to.toSet === p2.to.toSet &&
    attestationEq.eqv(p1.attestation, p2.attestation) &&
    p1.fee === p2.fee &&
    p1.timestamp === p2.timestamp &&
    p1.data === p2.data &&
    p1.minting === p2.minting

  implicit val tokenValueHolderEq: Eq[TokenValueHolder] = (t1, t2) => t1.quantity === t2.quantity

  implicit val securityRootEq: Eq[SecurityRoot] = (s1, s2) => s1.root === s2.root

  implicit val assetCodeEq: Eq[AssetCode] = (a1, a2) =>
    a1.version === a2.version && a1.issuer === a2.issuer && a1.shortName === a2.shortName

  implicit val assetValueEq: Eq[AssetValue] = (a1, a2) =>
    a1.quantity === a2.quantity &&
    a1.assetCode === a2.assetCode &&
    a1.securityRoot === a2.securityRoot &&
    a1.metadata === a2.metadata

  implicit val assetTransferEq: Eq[AssetTransfer[_ <: Proposition]] = (a1, a2) =>
    a1.from.toSet === a2.from.toSet &&
    a1.to.toSet === a2.to.toSet &&
    attestationEq.eqv(a1.attestation, a2.attestation) &&
    a1.fee === a2.fee &&
    a1.timestamp === a2.timestamp &&
    a1.data === a2.data &&
    a1.minting === a2.minting

  implicit val modifierIdEq: Eq[ModifierId] = (m1, m2) => m1.value === m2.value

  implicit val transactionEq: Eq[Transaction.TX] = (t1, t2) =>
    txBinaryShow.encodeAsBytes(t1) === txBinaryShow.encodeAsBytes(t2)

  implicit val blockEq: Eq[Block] = (b1, b2) =>
    b1.parentId === b2.parentId &&
    b1.timestamp === b2.timestamp &&
    b1.publicKey === b2.publicKey &&
    b1.signature === b2.signature &&
    b1.height === b2.height &&
    b1.difficulty === b2.difficulty &&
    b1.transactions === b2.transactions &&
    b1.version === b2.version

  implicit val arbitBoxEq: Eq[ArbitBox] = (a1, a2) =>
    a1.evidence === a2.evidence &&
    a1.nonce === a2.nonce &&
    a1.value === a2.value

  implicit val assetBoxEq: Eq[AssetBox] = (a1, a2) =>
    a1.evidence === a2.evidence &&
    a1.nonce === a2.nonce &&
    a1.value === a2.value

  implicit val polyBoxEq: Eq[PolyBox] = (p1, p2) =>
    p1.evidence === p2.evidence &&
    p1.nonce === p2.nonce &&
    p1.value === p2.value

  implicit val bloomFilterEq: Eq[BloomFilter] = (b1, b2) => b1.value === b2.value

  implicit val blockHeaderEq: Eq[BlockHeader] = (h1, h2) =>
    h1.id === h2.id &&
    h1.parentId === h2.parentId &&
    h1.timestamp === h2.timestamp &&
    h1.generatorBox === h2.generatorBox &&
    h1.publicKey === h2.publicKey &&
    h1.signature === h2.signature &&
    h1.height === h2.height &&
    h1.difficulty === h2.difficulty &&
    h1.txRoot === h2.txRoot &&
    h1.bloomFilter === h2.bloomFilter &&
    h1.version === h2.version

  implicit val blockBodyEq: Eq[BlockBody] = (b1, b2) =>
    b1.id === b2.id &&
    b1.parentId === b2.parentId &&
    b1.transactions === b2.transactions &&
    b1.version === b2.version

  implicit val boxEq: Eq[Box[_]] = (b1, b2) => boxBinaryShow.encodeAsBytes(b1) === boxBinaryShow.encodeAsBytes(b2)

  implicit val programIdEq: Eq[ProgramId] = (p1, p2) => p1.hashBytes === p2.hashBytes

//  implicit val codeBoxEq: Eq[CodeBox] = (c1, c2) =>
//    c1.evidence === c2.evidence &&
//    c1.nonce === c2.nonce &&
//    c1.value === c2.value &&
//    c1.code === c2.code &&
//    c1.interface === c2.interface
//
//  implicit val stateBoxEq: Eq[StateBox] = (s1, s2) =>
//    s1.evidence === s2.evidence &&
//    s1.nonce === s2.nonce &&
//    s1.value === s2.value &&
//    s1.state === s2.state
//
//  implicit val executionBoxEq: Eq[ExecutionBox] = (e1, e2) =>
//    e1.evidence === e2.evidence &&
//    e1.nonce === e2.nonce &&
//    e1.value === e2.value &&
//    e1.stateBoxIds === e2.stateBoxIds &&
//    e1.codeBoxIds === e2.codeBoxIds
}
