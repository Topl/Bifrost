package examples.bifrost.transaction

import java.io.{ByteArrayInputStream, ObjectInputStream}

import com.google.common.primitives.{Bytes, Ints, Longs}
import examples.bifrost.contract.{Agreement, AgreementTerms, PiecewiseLinearMultiple, PiecewiseLinearSingle}
import io.circe.{Json, ParsingFailure}
import io.circe.parser.parse
import scorex.core.serialization.Serializer
import scorex.core.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import cats.syntax.either._
import examples.hybrid.state.SimpleBoxTransactionCompanion
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.signatures.Curve25519

import scala.collection.immutable.HashMap
import scala.util.Try

object BifrostTransactionCompanion extends Serializer[BifrostTransaction] {

  override def toBytes(m: BifrostTransaction): Array[Byte] = m match {
    case c: ContractTransaction => ContractTransactionCompanion.toBytes(c)
    case p: PaymentTransaction => PaymentTransactionCompanion.toBytes(p)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

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

  override def toBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

    val agreementBytes = AgreementCompanion.toBytes(m.agreement)

    // TODO this might need a nonce
      Bytes.concat(
        Longs.toByteArray(m.fee),
        Longs.toByteArray(m.timestamp),
        Longs.toByteArray(agreementBytes.length),
        Ints.toByteArray(typeBytes.length),
        Ints.toByteArray(m.signatures.length),
        Ints.toByteArray(m.parties.length),
        agreementBytes,
        typeBytes,
        m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
        m.parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes)
      )
  }

  // TODO fix this
  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {
    val Array(fee: Long, timestamp: Long, agreementLength: Long) = (0 until 3).map { i =>
      Longs.fromByteArray(bytes.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    val numUsedBytes = 3*Longs.BYTES

    val Array(typeLength: Int, sigLength: Int, partiesLength: Int) = (0 until 3).map { i =>
      Ints.fromByteArray(bytes.slice(numUsedBytes, numUsedBytes + Ints.BYTES))
    }.toArray

    val postLengthUsedBytes = numUsedBytes + 3*Ints.BYTES

    val agreement = AgreementCompanion.parseBytes(
      bytes.slice(
        postLengthUsedBytes,
        postLengthUsedBytes + agreementLength.toInt
      )
    ).get

    val transType = new String(
      bytes.slice(
        postLengthUsedBytes + agreementLength.toInt,
        postLengthUsedBytes + typeLength + agreementLength.toInt
      )
    )

    val sigStart = postLengthUsedBytes + typeLength

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(sigStart + i * Curve25519.SignatureLength, sigStart + (i + 1) * Curve25519.SignatureLength))
    }

    val s = sigStart + sigLength * Curve25519.SignatureLength

    val parties = (0 until partiesLength) map { i =>
      val pk = bytes.slice(s + i * Curve25519.KeyLength, s + (i + 1) * Curve25519.KeyLength )
      PublicKey25519Proposition(pk)
    }

    ContractCreation(agreement, parties, signatures, fee, timestamp)
  }

}

object AgreementCompanion extends Serializer[Agreement] {

  override def toBytes(a: Agreement): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.nonce),
      Longs.toByteArray(a.timestamp),
      Longs.toByteArray(a.expirationTimestamp),
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      Ints.toByteArray(a.parties.length),
      a.parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes),
      a.terms.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {

    val Array(nonce: Long, timestamp: Long, expirationTimestamp: Long, termsLength: Long) = (0 until 4).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 4*Longs.BYTES

    val partiesLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))

    val parties = (0 until partiesLength) map { i =>
      val pk = bytes.slice(numBytesRead + Ints.BYTES + i * Curve25519.KeyLength, numBytesRead + Ints.BYTES + (i + 1) * Curve25519.KeyLength )
      PublicKey25519Proposition(pk)
    }

    numBytesRead += Ints.BYTES + partiesLength * Constants25519.PubKeyLength

    val termsMap: Map[String, Json] = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(x) => new HashMap[String, Json]()
      case Right(x) => x.asObject.get.toMap
    }

    val fulfilmentMap = termsMap("fulfilment").asObject.get.toMap
    val shareMap = termsMap("share").asObject.get.toMap

    val terms = new AgreementTerms(
      termsMap("pledge").as[BigDecimal].right.get,
      termsMap("xrate").as[BigDecimal].right.get,
      shareMap("functionType").asString.get match {
        case "PiecewiseLinearMultiple" => new PiecewiseLinearMultiple(
          shareMap("points").as[Seq[(Double,(Double, Double, Double))]].right.get
        )
      },
      fulfilmentMap("functionType").asString.get match {
        case "PiecewiseLinearSingle" => new PiecewiseLinearSingle(
          fulfilmentMap("points").as[Seq[(Long, Double)]].right.get)
      }
    )

    Agreement(parties, terms, nonce, timestamp, expirationTimestamp)
  }
}


object BifrostPaymentCompanion extends Serializer[BifrostPayment] {

  override def toBytes(m: BifrostPayment): Array[Byte] = BifrostTransactionCompanion.toBytes(m)

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