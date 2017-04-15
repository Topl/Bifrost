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
    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    typeStr match {
      case "ContractTransaction" => ContractTransactionCompanion.parseBytes(bytes).asInstanceOf[BifrostTransaction]
      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).asInstanceOf[BifrostTransaction]
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

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,Ints.BYTES + typeLength))

    /* Grab the rest of the bytes, which should begin similarly (with sub-type) */
    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.take(Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES,  Ints.BYTES + newTypeLength))

      newTypeStr match {
        case "ContractCreation" => ContractCreationCompanion.parseBytes(newBytes).get
      }
  }
}

object TransferTransactionCompanion extends Serializer[TransferTransaction] {

  override def toBytes(m: TransferTransaction): Array[Byte] = {
    val typeBytes = "TransferTransaction".getBytes

    Ints.toByteArray(typeBytes.length) ++
      typeBytes ++
      (m match {
        case sc: StableCoinTransfer => StableCoinTransferCompanion.toBytes(sc)
      })
  }

  override def parseBytes(bytes: Array[Byte]): Try[TransferTransaction] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    val newBytes = bytes.slice(Ints.BYTES + typeLength, bytes.length)

    val newTypeLength = Ints.fromByteArray(newBytes.slice(0, Ints.BYTES))
    val newTypeStr = new String(newBytes.slice(Ints.BYTES, Ints.BYTES + newTypeLength))

    newTypeStr match {
      case "StableCoinTransfer" => StableCoinTransferCompanion.parseBytes(newBytes).get.asInstanceOf[TransferTransaction]
    }
  }
}

object ContractCreationCompanion extends Serializer[ContractCreation] {

  override def toBytes(m: ContractCreation): Array[Byte] = {
    val typeBytes = "ContractCreation".getBytes

    val agreementBytes = AgreementCompanion.toBytes(m.agreement)

    // TODO this might need a nonce
      Bytes.concat(
        /* First two arguments MUST STAY */
        Ints.toByteArray(typeBytes.length),
        typeBytes,
        Longs.toByteArray(m.fee),
        Longs.toByteArray(m.timestamp),
        Longs.toByteArray(agreementBytes.length),
        Ints.toByteArray(m.signatures.length),
        Ints.toByteArray(m.parties.length),
        agreementBytes,
        m.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
        m.parties.foldLeft(Array[Byte]())((a, b) => a ++ b.pubKeyBytes)
      )
  }

  override def parseBytes(bytes: Array[Byte]): Try[ContractCreation] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val Array(fee: Long, timestamp: Long, agreementLength: Long) = (0 until 3).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i*Longs.BYTES, (i + 1)*Longs.BYTES))
    }.toArray

    numReadBytes = 3*Longs.BYTES

    val Array(sigLength: Int, partiesLength: Int) = (0 until 2).map { i =>
      Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i*Ints.BYTES, numReadBytes + (i + 1)*Ints.BYTES))
    }.toArray

    numReadBytes += 2*Ints.BYTES

    val agreement = AgreementCompanion.parseBytes(
      bytesWithoutType.slice(
        numReadBytes,
        numReadBytes + agreementLength.toInt
      )
    ).get

    numReadBytes += agreementLength.toInt

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytesWithoutType.slice(numReadBytes + i * Curve25519.SignatureLength, numReadBytes + (i + 1) * Curve25519.SignatureLength))
    }

    numReadBytes += sigLength * Curve25519.SignatureLength

    val parties = (0 until partiesLength) map { i =>
      val pk = bytesWithoutType.slice(numReadBytes + i * Curve25519.KeyLength, numReadBytes + (i + 1) * Curve25519.KeyLength )
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
      termsMap("pledge").asNumber.get.toLong.get,
      BigDecimal(termsMap("xrate").asString.get),
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

object StableCoinTransferCompanion extends Serializer[StableCoinTransfer] {

  override def toBytes(sc: StableCoinTransfer): Array[Byte] = {
    val typeBytes = "StableCoinTransfer".getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(sc.fee),
      Longs.toByteArray(sc.timestamp),
      Ints.toByteArray(sc.signatures.length),
      Ints.toByteArray(sc.from.length),
      Ints.toByteArray(sc.to.length),
      sc.signatures.foldLeft(Array[Byte]())((a,b) => a ++ b.bytes),
      sc.from.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      sc.to.foldLeft(Array[Byte]())((a,b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[StableCoinTransfer] = Try {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES,  Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2*Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice( numBytesRead + 2*Longs.BYTES, numBytesRead + 2*Longs.BYTES + Ints.BYTES))

    numBytesRead += 2*Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2*Ints.BYTES))

    numBytesRead += 2*Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(numBytesRead + i * Curve25519.SignatureLength, numBytesRead + (i + 1) * Curve25519.SignatureLength))
    }

    numBytesRead += sigLength*Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1)*elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i*elementLength, numBytesRead + (i + 1)*elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1)*elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    StableCoinTransfer(from, to, signatures, fee, timestamp)
  }
}