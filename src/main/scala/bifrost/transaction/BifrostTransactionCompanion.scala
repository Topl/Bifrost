package bifrost.transaction

import bifrost.contract._
import bifrost.contract.modules.BaseModuleWrapper
import bifrost.transaction.BifrostTransaction.Nonce
import bifrost.transaction.Role.Role
import bifrost.transaction.box.{ContractBox, ContractBoxSerializer, ReputationBox}
import com.google.common.primitives.{Bytes, Doubles, Ints, Longs}
import io.circe.optics.JsonPath._
import io.circe.parser._
import io.circe.{HCursor, Json}
import io.iohk.iodb.ByteArrayWrapper
import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction._
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519
import serialization._

import scala.util.Try

object BifrostTransactionCompanion extends Serializer[BifrostTransaction] {

  override def toBytes(m: BifrostTransaction): Array[Byte] = m match {
    case c: ContractTransaction => ContractTransactionCompanion.toBytes(c)
    case p: TransferTransaction => TransferTransactionCompanion.toBytes(p)
    case r: ProfileTransaction => ProfileTransactionCompanion.toBytes(r)
    case ar: AssetRedemption => AssetRedemptionCompanion.toBytes(ar)
    //case ct: ConversionTransaction => ConversionTransactionCompanion.toBytes(ct)
    case ac: AssetCreation => AssetCreationCompanion.toBytes(ac)
    case cb: CoinbaseTransaction => CoinbaseTransactionCompanion.toBytes(cb)
  }

  override def parseBytes(bytes: Array[Byte]): Try[BifrostTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.slice(0, Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    typeStr match {
      case "ContractTransaction" => ContractTransactionCompanion.parseBytes(bytes).get
      case "TransferTransaction" => TransferTransactionCompanion.parseBytes(bytes).get
      case "ProfileTransaction" => ProfileTransactionCompanion.parseBytes(bytes).get
      case "AssetRedemption" => AssetRedemptionCompanion.parseBytes(bytes).get
      //case "ConversionTransaction" => ConversionTransactionCompanion.parseBytes(bytes).get
      case "AssetCreation" => AssetCreationCompanion.parseBytes(bytes).get
      case "CoinbaseTransaction" => CoinbaseTransactionCompanion.parseBytes(bytes).get
    }
  }

}









object AgreementCompanion extends Serializer[Agreement] {

  override def toBytes(a: Agreement): Array[Byte] = {
    Bytes.concat(
      Longs.toByteArray(a.terms.json.noSpaces.getBytes.length),
      Longs.toByteArray(a.core.json.noSpaces.getBytes.length),
      Ints.toByteArray(a.assetCode.getBytes.length),
      a.assetCode.getBytes,
      a.terms.json.noSpaces.getBytes,
      a.core.json.noSpaces.getBytes
    )
  }

  override def parseBytes(bytes: Array[Byte]): Try[Agreement] = Try {

    val Array(termsLength: Long, coreLength: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytes.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    var numBytesRead = 2 * Longs.BYTES

    val numStrBytes = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))

    numBytesRead += Ints.BYTES

    val assetCode: String = new String(bytes.slice(numBytesRead, numBytesRead + numStrBytes))

    numBytesRead += numStrBytes

    val terms: AgreementTerms = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + termsLength.toInt)
    )) match {
      case Left(_) => throw new Exception("AgreementTerm json not properly formatted")
      case Right(x) => x.as[AgreementTerms] match {
        case Left(_) => throw new Exception("Agreement terms json was malformed")
        case Right(a: AgreementTerms) => a
      }
    }

    numBytesRead += termsLength.toInt

    val core: BaseModuleWrapper = parse(new String(
      bytes.slice(numBytesRead, numBytesRead + coreLength.toInt)
    )) match {
      case Left(_) => throw new Exception("BaseModule json not properly formatted")
      case Right(x) => x.as[BaseModuleWrapper] match {
        case Left(_) => throw new Exception("Internal json was malformed in BaseModule")
        case Right(b: BaseModuleWrapper) => b
      }
    }

    Agreement(terms, assetCode, core)
  }
}

trait TransferSerializer {
  def transferToBytes(tx: TransferTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(tx.fee),
      Longs.toByteArray(tx.timestamp),
      Ints.toByteArray(tx.signatures.length),
      Ints.toByteArray(tx.from.length),
      Ints.toByteArray(tx.to.length),
      tx.signatures.foldLeft(Array[Byte]())((a, b) => a ++ b.bytes),
      tx.from.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2)),
      tx.to.foldLeft(Array[Byte]())((a, b) => a ++ b._1.pubKeyBytes ++ Longs.toByteArray(b._2))
    )
  }

  /*def conversionToBytes(ct: ConversionTransaction, txType: String): Array[Byte] = {
    val typeBytes = txType.getBytes

    // concatenate map keys to reduce size when assetCodes are the same
    val keySeq = (ct.totalAssetBoxes.keySet ++ ct.assetsToReturn.keySet ++
      ct.assetTokensToRedeem.keySet ++ ct.conversionSignatures.keySet).toSeq.zipWithIndex
    val keyMapping: Map[(String, PublicKey25519Proposition), Int] = keySeq.toMap

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Ints.toByteArray(ct.data.length),
      ct.data.getBytes,
      Longs.toByteArray(ct.fee),
      Longs.toByteArray(ct.timestamp),
      Ints.toByteArray(ct.totalAssetBoxes.size),
      Ints.toByteArray(ct.assetsToReturn.size),
      Ints.toByteArray(ct.assetTokensToRedeem.size),
      Ints.toByteArray(ct.conversionSignatures.size),
      Ints.toByteArray(keyMapping.size),
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(b._1._1.getBytes.length) ++ b._1._1.getBytes ++ b
        ._1._2.pubKeyBytes),
      ct.totalAssetBoxes.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(totAssets => totAssets._1.pubKeyBytes ++ Longs.toByteArray(
        totAssets._2))
      ),
      ct.assetsToReturn.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetReturn => assetReturn._1.pubKeyBytes ++ Longs.toByteArray(
        assetReturn._2))
      ),
      ct.assetTokensToRedeem.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(assetRedeem => assetRedeem._1.pubKeyBytes ++ Longs.toByteArray(
        assetRedeem._2))
      ),
      ct.conversionSignatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(_.signature)
      )
    )
  }*/

  def parametersParseBytes(bytes: Array[Byte]): (IndexedSeq[(PublicKey25519Proposition, Long)],
    IndexedSeq[(PublicKey25519Proposition, Long)],
    IndexedSeq[Signature25519], Long, Long) = {

    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))

    var numBytesRead = Ints.BYTES + typeLength

    val fee = Longs.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Longs.BYTES))
    val timestamp = Longs.fromByteArray(bytes.slice(numBytesRead + Longs.BYTES, numBytesRead + 2 * Longs.BYTES))
    val sigLength = Ints.fromByteArray(bytes.slice(numBytesRead + 2 * Longs.BYTES,
      numBytesRead + 2 * Longs.BYTES + Ints.BYTES))

    numBytesRead += 2 * Longs.BYTES + Ints.BYTES

    val fromLength = Ints.fromByteArray(bytes.slice(numBytesRead, numBytesRead + Ints.BYTES))
    val toLength = Ints.fromByteArray(bytes.slice(numBytesRead + Ints.BYTES, numBytesRead + 2 * Ints.BYTES))

    numBytesRead += 2 * Ints.BYTES

    val signatures = (0 until sigLength) map { i =>
      Signature25519(bytes.slice(numBytesRead + i * Curve25519.SignatureLength,
        numBytesRead + (i + 1) * Curve25519.SignatureLength))
    }

    numBytesRead += sigLength * Curve25519.SignatureLength

    val elementLength = Longs.BYTES + Curve25519.KeyLength

    val from = (0 until fromLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val nonce = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), nonce)
    }

    numBytesRead += fromLength * elementLength
    val to = (0 until toLength) map { i =>
      val pk = bytes.slice(numBytesRead + i * elementLength, numBytesRead + (i + 1) * elementLength - Longs.BYTES)
      val v = Longs.fromByteArray(
        bytes.slice(numBytesRead + (i + 1) * elementLength - Longs.BYTES, numBytesRead + (i + 1) * elementLength)
      )
      (PublicKey25519Proposition(pk), v)
    }
    (from, to, signatures, fee, timestamp)
  }
}













/*object ConversionTransactionCompanion extends Serializer[ConversionTransaction] with TransferSerializer {

  override def toBytes(ct: ConversionTransaction): Array[Byte] = {
    conversionToBytes(ct, "ConversionTransaction")
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[ConversionTransaction] = Try {
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)

    val dataLength = Ints.fromByteArray(bytesWithoutType.take(Ints.BYTES))
    val data = new String(bytesWithoutType.slice(Ints.BYTES, Ints.BYTES + dataLength))

    numReadBytes = dataLength * Ints.BYTES + Ints.BYTES

    //read in byte stream to array for fee and timestamp
    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    numReadBytes += 2 * Longs.BYTES

    val Array(totalAssetLength: Int,
    assetReturnLength: Int,
    assetRedeemLength: Int,
    sigLength: Int,
    keyMappingSize: Int) =
      (0 until 5).map { i =>
        Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i * Ints.BYTES, numReadBytes + (i + 1) * Ints.BYTES))
      }.toArray

    numReadBytes += 5 * Ints.BYTES

    val keyMapping: Map[Int, (String, PublicKey25519Proposition)] = (0 until keyMappingSize).map { i =>
      val ahLen = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
      val asset = new String(bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + ahLen))

      numReadBytes += Ints.BYTES + ahLen

      val hub = PublicKey25519Proposition(
        bytesWithoutType.slice(
          numReadBytes,
          numReadBytes + Constants25519.PubKeyLength
        )
      )

      numReadBytes += Constants25519.PubKeyLength

      i -> (asset, hub)
    }.toMap

    val totalAssets: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Nonce)]] =
      (0 until totalAssetLength).map { _ =>
        var bytesSoFar = 0
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

        bytesSoFar = Ints.BYTES

        val boxesLength = Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes + bytesSoFar, numReadBytes + bytesSoFar + Ints.BYTES)
        )

        bytesSoFar += Ints.BYTES

        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

        val boxes: IndexedSeq[(PublicKey25519Proposition, Nonce)] = (0 until boxesLength).map { j =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              numReadBytes + bytesSoFar + j * chunkSize,
              numReadBytes + bytesSoFar + j * chunkSize + Constants25519.PubKeyLength
            )
          )

          val nonceStart = numReadBytes + bytesSoFar + j * chunkSize + Constants25519.PubKeyLength
          val nonce = Longs.fromByteArray(bytesWithoutType.slice(nonceStart, nonceStart + Longs.BYTES))

          prop -> nonce
        }

        bytesSoFar += boxesLength * chunkSize
        numReadBytes += bytesSoFar

        assetHub -> boxes
      }.toMap

    val assetReturn: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]] =
      (0 until assetReturnLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

        val retLength = Ints.fromByteArray(bytesWithoutType.slice(
          numReadBytes + Ints.BYTES, numReadBytes + 2 * Ints.BYTES))

        val startPosition = numReadBytes + 2 * Ints.BYTES
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

        val retSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until retLength).map { i =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              startPosition + i * chunkSize,
              startPosition + i * chunkSize + Constants25519.PubKeyLength)
          )

          val amount = Longs.fromByteArray(
            bytesWithoutType.slice(
              startPosition + i * chunkSize + Constants25519.PubKeyLength,
              startPosition + (i + 1) * chunkSize)
          )

          prop -> amount
        }

        numReadBytes += 2 * Ints.BYTES + retLength * chunkSize
        assetHub -> retSeq
      }.toMap

    val assetRedeem: Map[(String, PublicKey25519Proposition), IndexedSeq[(PublicKey25519Proposition, Long)]] =
      (0 until assetRedeemLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

        val redeemLength = Ints.fromByteArray(bytesWithoutType.slice(
          numReadBytes + Ints.BYTES, numReadBytes + 2 * Ints.BYTES))

        val startPosition = numReadBytes + 2 * Ints.BYTES
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

        val redeemSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until redeemLength).map { i =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(
              startPosition + i * chunkSize,
              startPosition + i * chunkSize + Constants25519.PubKeyLength)
          )

          val amount = Longs.fromByteArray(
            bytesWithoutType.slice(
              startPosition + i * chunkSize + Constants25519.PubKeyLength,
              startPosition + (i + 1) * chunkSize)
          )

          prop -> amount
        }

        numReadBytes += 2 * Ints.BYTES + redeemLength * chunkSize
        assetHub -> redeemSeq
      }.toMap

    val signatures: Map[(String, PublicKey25519Proposition), IndexedSeq[Signature25519]] =
      (0 until sigLength).map { _ =>
        val assetHub = keyMapping(Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

        val numSigs = Ints.fromByteArray(
          bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + 2 * Ints.BYTES)
        )

        val sigs: IndexedSeq[Signature25519] = (0 until numSigs).map { j =>
          Signature25519(
            bytesWithoutType.slice(
              numReadBytes + j * Curve25519.SignatureLength + 2 * Ints.BYTES,
              numReadBytes + (j + 1) * Curve25519.SignatureLength + 2 * Ints.BYTES
            )
          )
        }

        numReadBytes += 2 * Ints.BYTES + numSigs * Curve25519.SignatureLength
        assetHub -> sigs
      }.toMap

    ConversionTransaction(totalAssets, assetReturn, assetRedeem, signatures, fee, timestamp, data)
  }
}*/
