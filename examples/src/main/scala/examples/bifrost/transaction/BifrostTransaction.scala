package examples.bifrost.transaction

import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.transaction.contract._
import examples.curvepos.transaction.PublicKey25519NoncedBox
import examples.hybrid.state.SimpleBoxTransaction
import examples.hybrid.state.SimpleBoxTransaction.Nonce
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.core.transaction.{BoxTransaction, Transaction}
import scorex.crypto.encode.Base58

sealed trait BifrostTransaction extends BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox]

sealed abstract class ContractTransaction extends BifrostTransaction

// 3 signatures FOR A SPECIFIC MESSAGE <agreement: Agreement>
// ContractCreation(agreement ++ nonce, IndexSeq(pk1, pk2, pk3), IndexSeq(sign1(agreement ++ nonce), sign2(agreement ++ nonce), sign3(agreement ++ nonce)) )
// validity check: decrypt[pk1] sign1(agreement) === agreement
// agreement specifies "executeBy" date
case class ContractCreation(agreement: Agreement,
                            parties: IndexedSeq[PublicKey25519Proposition],
                            signatures: IndexedSeq[Signature25519],
                            fee: Long,
                            timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

  // no boxes required for now -- will require reputation
  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = IndexedSeq[Array[Byte]]()

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )


  // TODO generate 3 contract boxes, one for each participant as "authorization"
  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = parties.zipWithIndex.map { case (prop, idx) =>
    val nonce = nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
    PublicKey25519NoncedBox(prop, nonce, value)
  }

  override lazy val json: Json = Map(
    "agreement" -> agreement.json,
    "parties" -> parties.map( p => Base58.encode(p.pubKeyBytes).asJson ).asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"ContractCreation(${json.noSpaces})"

}

object ContractCreation {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): SimpleBoxTransaction = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = ContractCreation(, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new SimpleBoxTransaction(fromPub, to, sigs, fee, timestamp)
  }
}


case class Agreement(parties: IndexedSeq[PublicKey25519Proposition],
                     terms: AgreementTerms,
                     nonce: Long,
                     timestamp: Long,
                     expirationTimestamp: Long) {

  lazy val json: Json = Map(
    "parties" -> Array( parties.map(p => Base58.encode(p.pubKeyBytes)) ).asJson,
    "terms" -> terms.json,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson,
    "expirationTimestamp" -> expirationTimestamp.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.noSpaces})"

}


trait PaymentTransaction extends BifrostTransaction

case class BifrostPayment(override val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                          override val to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          override val signatures: IndexedSeq[Signature25519],
                          override val fee: Long,
                          override val timestamp: Long)
  extends SimpleBoxTransaction(from, to, signatures, fee, timestamp) with PaymentTransaction {

  override type M = BifrostPayment

  override lazy val serializer = BifrostPaymentCompanion

  override def toString: String = s"BifrostPayment(${json.noSpaces})"
}