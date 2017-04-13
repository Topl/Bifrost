package examples.bifrost.transaction

import com.google.common.primitives.{Ints, Longs}
import examples.bifrost.transaction.BifrostPayment.Nonce
import examples.bifrost.contract._
import examples.bifrost.transaction.box.{BifrostBox, BifrostPaymentBox, ContractBox, PublicKey25519NoncedBox}
import examples.hybrid.wallet.HWallet
import io.circe.Json
import io.circe.syntax._
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.transaction.account.PublicKeyNoncedBox
import scorex.core.transaction.box.BoxUnlocker
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.{Proof, Signature25519}
import scorex.core.transaction.state.{PrivateKey25519, PrivateKey25519Companion}
import scorex.crypto.encode.Base58

import scala.util.Try

sealed trait BifrostTransaction extends GenericBoxTransaction[PublicKey25519Proposition, Any, BifrostBox]

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
    AgreementCompanion.toBytes(agreement) ++
      parties.foldLeft(Array[Byte]())((a,b) => a ++ b.pubKeyBytes) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )


  // TODO generate 3 contract boxes, one for each participant as "authorization"
  override lazy val newBoxes: Traversable[BifrostBox] = parties.zipWithIndex.map {
    case (prop, idx) =>
      val nonce = ContractCreation.nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      val newContractId = new String(FastCryptographicHash(ContractCreationCompanion.toBytes(this)))
      ContractBox(prop, nonce, newContractId)
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

case class BifrostPayment(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
                          to: IndexedSeq[(PublicKey25519Proposition, Long)],
                          signatures: IndexedSeq[Signature25519],
                          override val fee: Long,
                          override val timestamp: Long)
  extends PaymentTransaction {

  override type M = BifrostPayment

  override lazy val serializer = BifrostPaymentCompanion

  override def toString: String = s"BifrostPayment(${json.noSpaces})"

  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
    case (boxId, signature) =>
      new BoxUnlocker[PublicKey25519Proposition] {
        override val closedBoxId: Array[Byte] = boxId
        override val boxKey: Proof[PublicKey25519Proposition] = signature
      }
  }

  lazy val hashNoNonces = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
      Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee)
  )

  override lazy val newBoxes: Traversable[BifrostBox] = to.zipWithIndex.map {
    case ((prop, value), idx) =>
      val nonce = BifrostPayment.nonceFromDigest(FastCryptographicHash(prop.pubKeyBytes ++ hashNoNonces ++ Ints.toByteArray(idx)))
      BifrostPaymentBox(prop, nonce, value)
  }

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
    "from" -> from.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "nonce" -> s._2.asJson
      ).asJson
    }.asJson,
    "to" -> to.map { s =>
      Map(
        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
        "value" -> s._2.asJson
      ).asJson
    }.asJson,
    "signatures" -> signatures.map(s => Base58.encode(s.signature).asJson).asJson,
    "fee" -> fee.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson
}

object BifrostPayment {
  type Value = Long
  type Nonce = Long

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def apply(from: IndexedSeq[(PrivateKey25519, Nonce)],
            to: IndexedSeq[(PublicKey25519Proposition, Value)],
            fee: Long,
            timestamp: Long): BifrostPayment = {
    val fromPub = from.map { case (pr, n) => pr.publicImage -> n }
    val fakeSigs = from.map(_ => Signature25519(Array()))

    val undersigned = BifrostPayment(fromPub, to, fakeSigs, fee, timestamp)

    val msg = undersigned.messageToSign
    val sigs = from.map { case (priv, _) => PrivateKey25519Companion.sign(priv, msg) }

    new BifrostPayment(fromPub, to, sigs, fee, timestamp)
  }

  //TODO seq of recipients and amounts
  def create(w: HWallet, recipient: PublicKey25519Proposition, amount: Long, fee: Long): Try[BifrostPayment] = Try {

    val from: IndexedSeq[(PrivateKey25519, Long, Long)] = w.boxes().flatMap { b =>
      w.secretByPublicImage(b.box.proposition).map(s => (s, b.box.nonce, b.box.value))
    }.toIndexedSeq
    val canSend = from.map(_._3).sum
    val charge: (PublicKey25519Proposition, Long) = (w.publicKeys.head, canSend - amount - fee)

    val to: IndexedSeq[(PublicKey25519Proposition, Long)] = IndexedSeq(charge, (recipient, amount))

    require(from.map(_._3).sum - to.map(_._2).sum == fee)

    val timestamp = System.currentTimeMillis()
    BifrostPayment(from.map(t => t._1 -> t._2), to, fee, timestamp)
  }

}