package examples.bifrost.transaction.contract

import java.io.{ByteArrayInputStream, ObjectInputStream}

import com.google.common.primitives.Longs
import io.circe.Json
import io.circe.syntax._
import io.circe.parser._
import io.circe.generic.auto._
import scorex.core.serialization.Serializer
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import scorex.crypto.encode.Base58
import cats.syntax.either._

import scala.util.Try

sealed trait ContractTransaction extends Transaction[PublicKey25519Proposition]

case class ContractCreation(sender: PublicKey25519Proposition,
                     agreement: Agreement,
                     fee: Long,
                     nonce: Long,
                     timestamp: Long)
  extends ContractTransaction {

  override type M = ContractCreation

  override lazy val json: Json = Map(
    "sender" -> Base58.encode(sender.pubKeyBytes).asJson,
    "agreement" -> agreement.json,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = ContractCreationCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"ContractCreation(${json.noSpaces})"

}

object ContractCreationCompanion extends Serializer[ContractCreation] {
  val MaxTransactionLength: Int = Constants25519.PubKeyLength + AgreementCompanion.MaxTransactionLength + 24

  override def toBytes(m: ContractCreation): Array[Byte] = {
    m.sender.bytes ++
      AgreementCompanion.toBytes(m.agreement) ++
      Longs.toByteArray(m.fee) ++
      Longs.toByteArray(m.nonce) ++
      Longs.toByteArray(m.timestamp)
  }.ensuring(_.length < MaxTransactionLength)

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {
    val sender = PublicKey25519Proposition(bytes.slice(0, Constants25519.PubKeyLength))

    val agreement = AgreementCompanion.parseBytes(
      bytes.slice(Constants25519.PubKeyLength + 1, Constants25519.PubKeyLength + 1 + bytes(Constants25519.PubKeyLength).toInt)
    ).get

    val s = Constants25519.PubKeyLength + bytes(Constants25519.PubKeyLength).toInt
    val fee = Longs.fromByteArray(bytes.slice(s, s + 8))
    val nonce = Longs.fromByteArray(bytes.slice(s + 8, s + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(s + 16, s + 24))
    ContractCreation(sender, agreement, fee, nonce, timestamp)
  }
}

case class Agreement(senders: (PublicKey25519Proposition, PublicKey25519Proposition, PublicKey25519Proposition),
                     terms: AgreementTerms,
                     nonce: Long,
                     fee: Long = 0,
                     timestamp: Long)
  extends ContractTransaction {

  override type M = Agreement

  override lazy val json: Json = Map(
    "senders" -> Array(
      Base58.encode(senders._1.pubKeyBytes),
      Base58.encode(senders._2.pubKeyBytes),
      Base58.encode(senders._3.pubKeyBytes)).asJson,
    "terms" -> terms.json,
    "fee" -> fee.asJson,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  override lazy val serializer = AgreementCompanion

  override lazy val messageToSign: Array[Byte] = serializer.toBytes(this)

  override def toString: String = s"Agreement(${json.noSpaces})"

}

object AgreementCompanion extends Serializer[Agreement] {
  val MaxTransactionLength: Int = Constants25519.PubKeyLength*3 + 8 + 1024*100 + 24

  override def toBytes(m: Agreement): Array[Byte] = {
    m.senders._1.pubKeyBytes ++
      m.senders._2.pubKeyBytes ++
      m.senders._3.pubKeyBytes ++
      Longs.toByteArray(m.terms.json.toString.getBytes.length) ++
      m.terms.json.toString.getBytes  ++
      Longs.toByteArray(m.fee) ++
      Longs.toByteArray(m.nonce) ++
      Longs.toByteArray(m.timestamp)
  }.ensuring(_.length < MaxTransactionLength)

  private def deserialise(bytes: Array[Byte]): Any = {
    new ObjectInputStream(new ByteArrayInputStream(bytes)).readObject()
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {
    val senders = (PublicKey25519Proposition(bytes.slice(0, Constants25519.PubKeyLength)),
      PublicKey25519Proposition(bytes.slice(Constants25519.PubKeyLength, 2*Constants25519.PubKeyLength)),
      PublicKey25519Proposition(bytes.slice(2*Constants25519.PubKeyLength, 3*Constants25519.PubKeyLength)))

    val jsonLength = Longs.fromByteArray(bytes.slice(3*Constants25519.PubKeyLength, 3*Constants25519.PubKeyLength + 8))

    val termsMap: Map[String, Json] = parse(new String(
      bytes.slice(3*Constants25519.PubKeyLength + 1, 3*Constants25519.PubKeyLength + 1 + jsonLength.toInt)
    )).getOrElse(Json.Null).as[Map[String, Json]].right.get

    val terms = new AgreementTerms(
      termsMap("pledge").as[BigDecimal].right.get,
      termsMap("xrate").as[BigDecimal].right.get,
      this.deserialise(Base58.decode(termsMap("share").asString.get).get)
        .asInstanceOf[Function[Long, (BigDecimal, BigDecimal, BigDecimal)]],
      this.deserialise(Base58.decode(termsMap("fulfilment").asString.get).get)
        .asInstanceOf[Function[Long, BigDecimal]]
    )

    val s = Constants25519.PubKeyLength + 8
    val fee = Longs.fromByteArray(bytes.slice(s, s + 8))
    val nonce = Longs.fromByteArray(bytes.slice(s + 8, s + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(s + 16, s + 24))
    Agreement(senders, terms, fee, nonce, timestamp)
  }
}