package examples.bifrost.transaction

import java.io.{ByteArrayInputStream, ObjectInputStream}

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.bifrost.transaction.contract.{AgreementTerms, PiecewiseLinearMultiple, PiecewiseLinearSingle}
import io.circe.Json
import io.circe.parser.parse
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import cats.syntax.either._
import examples.hybrid.state.SimpleBoxTransactionCompanion
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object BifrostTransactionCompanion extends Serializer[BifrostTransaction] {

  override def toBytes(m: BifrostTransaction): Array[Byte] = m match {
    case c: ContractTransaction => ContractTransactionCompanion.toBytes(c)
    case p: PaymentTransaction => PaymentTransactionCompanion.toBytes(p)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, 4))
    val typeStr = new String(bytes.slice(4,4 + typeLength))

    typeStr match {
      case "ContractTransaction" => ContractTransactionCompanion.parseBytes(bytes).asInstanceOf[BifrostTransaction]
      case "PaymentTransaction" => PaymentTransactionCompanion.parseBytes(bytes).asInstanceOf[BifrostTransaction]
    }
  }

}

object ContractTransactionCompanion extends Serializer[ContractTransaction] {

  override def toBytes(m: ContractTransaction): Array[Byte] = {
    val typeBytes = "ContractTransaction".getBytes

    Ints.toByteArray(typeBytes.length) ++
      typeBytes ++
      (m match {
        case cc: ContractCreation => ContractCreationCompanion.toBytes(cc)
        case a: Agreement => AgreementCompanion.toBytes(a)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.slice(0, 4))
    val typeStr = new String(bytes.slice(4,4 + typeLength))
    val newBytes = bytes.slice(4 + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, 4))
    val newTypeStr = new String(newBytes.slice(4,4 + typeLength))

    newTypeStr match {
      case "ContractCreation" => ContractCreationCompanion.parseBytes(newBytes).asInstanceOf[ContractTransaction]
      case "Agreement" => AgreementCompanion.parseBytes(newBytes).asInstanceOf[ContractTransaction]
    }
  }
}

object PaymentTransactionCompanion extends Serializer[PaymentTransaction] {

  override def toBytes(m: PaymentTransaction): Array[Byte] = {
    val typeBytes = "PaymentTransaction".getBytes

    Ints.toByteArray(typeBytes.length) ++
      typeBytes ++
      (m match {
        case cc: ContractCreation => ContractCreationCompanion.toBytes(cc)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[PaymentTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.slice(0, 4))
    val typeStr = new String(bytes.slice(4,4 + typeLength))
    val newBytes = bytes.slice(4 + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, 4))
    val newTypeStr = new String(newBytes.slice(4,4 + typeLength))

    newTypeStr match {
      case "PaymentTransaction" => BifrostPaymentCompanion.parseBytes(newBytes).asInstanceOf[PaymentTransaction]
    }
  }
}

object ContractCreationCompanion extends Serializer[ContractCreation] {
  val MaxTransactionLength: Int = Constants25519.PubKeyLength + AgreementCompanion.MaxTransactionLength + 24

  override def toBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

      Bytes.concat(
        Longs.toByteArray(m.fee),
        Longs.toByteArray(m.timestamp),
        Ints.toByteArray(typeBytes.length),
        Ints.toByteArray(m.signatures.length),
        Ints.toByteArray(m.agreements.length),
        typeBytes,
        m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
        m.agreements.foldLeft(Array[Byte]())((a, b) => a ++ b._1.bytes ++ Longs.toByteArray(b._2))
      )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(Longs.BYTES, 2*Longs.BYTES))

    val numUsedBytes = 2*Longs.BYTES
    val typeLength = Ints.fromByteArray(bytes.slice(numUsedBytes, numUsedBytes + Ints.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice(numUsedBytes + Ints.BYTES, numUsedBytes + 2*Ints.BYTES))
    val agreementsLength = Ints.fromByteArray(bytes.slice(numUsedBytes + 2*Ints.BYTES, numUsedBytes + 3*Ints.BYTES))

    val postLengthUsedBytes = numUsedBytes + 3*Ints.BYTES

    val transType = new String(bytes.slice(postLengthUsedBytes, postLengthUsedBytes + typeLength))

    val sigStart = postLengthUsedBytes + typeLength

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(sigStart + i * Curve25519.SignatureLength, sigStart + (i + 1) * Curve25519.SignatureLength))
    }

    val s = sigStart + sigLength * Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val agreements = (0 until agreementsLength) map { i =>
      val pk = bytes.slice(s + i * elementLength, s + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s + (i + 1) * elementLength - 8, s + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), v)
    }

    ContractCreation(agreements, signatures, fee, timestamp)
  }

}

object AgreementCompanion extends Serializer[Agreement] {
  val MaxTransactionLength: Int = Constants25519.PubKeyLength*3 + 8 + 1024*10 + 24

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

    val fulfilmentMap = termsMap("fulfilment").as[Map[String, Json]].right
    val shareMap = termsMap("share").as[Map[String, Json]].right

    val terms = new AgreementTerms(
      termsMap("pledge").as[BigDecimal].right.get,
      termsMap("xrate").as[BigDecimal].right.get,
      shareMap.get("functionType").asString.get match {
        case "PiecewiseLinear" => new PiecewiseLinearMultiple(
          shareMap.get("points").as[Seq[(Double,(Double, Double, Double))]].right.get
        )
      },
      fulfilmentMap.get("functionType").asString.get match {
        case "PiecewiseLinear" => new PiecewiseLinearSingle(
          fulfilmentMap.get("points").as[Seq[(Long, Double)]].right.get)
      }
    )

    val s = Constants25519.PubKeyLength + 8
    val fee = Longs.fromByteArray(bytes.slice(s, s + 8))
    val nonce = Longs.fromByteArray(bytes.slice(s + 8, s + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(s + 16, s + 24))
    Agreement(senders, terms, fee, nonce, timestamp)
  }
}


object BifrostPaymentCompanion extends Serializer[BifrostPayment] {

  override def toBytes(m: BifrostPayment): Array[Byte] = SimpleBoxTransactionCompanion.toBytes(m)

  override def parseBytes(bytes: Array[Byte]): Try[BifrostPayment] = Try {
    val fee = Longs.fromByteArray(bytes.slice(0, Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(Longs.BYTES, 2*Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice(16, 20))
    val fromLength = Ints.fromByteArray(bytes.slice(20, 24))
    val toLength = Ints.fromByteArray(bytes.slice(24, 28))
    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(28 + i * Curve25519.SignatureLength, 28 + (i + 1) * Curve25519.SignatureLength))
    }
    val s = 28 + sigLength * Curve25519.SignatureLength
    val elementLength = 8 + Curve25519.KeyLength
    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(s + i * elementLength, s + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s + (i + 1) * elementLength - 8, s + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), v)
    }
    val s2 = s + fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(s2 + i * elementLength, s2 + (i + 1) * elementLength - 8)
      val v = Longs.fromByteArray(bytes.slice(s2 + (i + 1) * elementLength - 8, s2 + (i + 1) * elementLength))
      (PublicKey25519Proposition(pk), v)
    }
    BifrostPayment(from, to, signatures, fee, timestamp)
  }
}