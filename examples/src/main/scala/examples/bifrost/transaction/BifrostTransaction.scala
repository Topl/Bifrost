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
import scorex.core.transaction.{BoxTransaction, Transaction}
import scorex.crypto.encode.Base58

sealed trait BifrostTransaction extends BoxTransaction[PublicKey25519Proposition, PublicKey25519NoncedBox]


/** CONTRACT TRANSACTIONS **/

sealed abstract class ContractTransaction extends BifrostTransaction


// 3 signatures FOR A SPECIFIC MESSAGE <agreement: Array[Byte]>
// ContractCreation(agreement ++ nonce, IndexSeq(pk1, pk2, pk3), IndexSeq(sign1(agreement ++ nonce), sign2(agreement ++ nonce), sign3(agreement ++ nonce)) )
// validity check: decrypt[pk1] sign1(agreement) === agreement
// agreement specifies "executeBy" date
case class ContractCreation(agreement: Agreement,
                            parties: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                            signatures: IndexedSeq[Signature25519],
                            fee: Long,
                            timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

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


  // TODO generate 3 contract boxes, one for each participant
  override lazy val newBoxes: Traversable[PublicKey25519NoncedBox] = ???

  override lazy val json: Json = Map(
    "agreements" -> parties.map { a =>
      Map(
        "proposition" -> Base58.encode(a._1.pubKeyBytes).asJson,
        "nonce" -> a._2.asJson
      ).asJson
    }.asJson,
    "signature" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"ContractCreation(${json.noSpaces})"

}


case class Agreement(sender: PublicKey25519Proposition,
                     parties: IndexedSeq[PublicKey25519Proposition],
                     terms: AgreementTerms,
                     nonce: Long,
                     fee: Long = 0,
                     timestamp: Long) {

  lazy val json: Json = Map(
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "parties" -> Array( parties.map(p => Base58.encode(p.pubKeyBytes)) ).asJson,
    "terms" -> terms.json,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
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