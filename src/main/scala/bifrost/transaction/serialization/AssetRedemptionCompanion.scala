package bifrost.transaction.serialization

import bifrost.serialization.Serializer
import bifrost.transaction.bifrostTransaction.BifrostTransaction.Nonce
import bifrost.transaction.bifrostTransaction.AssetRedemption
import bifrost.transaction.box.proposition.{Constants25519, PublicKey25519Proposition}
import bifrost.transaction.proof.Signature25519
import com.google.common.primitives.{Bytes, Ints, Longs}
import scorex.crypto.signatures.Curve25519

import scala.util.Try

object AssetRedemptionCompanion extends Serializer[AssetRedemption] {
  override def toBytes(ac: AssetRedemption): Array[Byte] = {
    val typeBytes = "AssetRedemption".getBytes

    // Used to reduce overall size in the default case where assetcodes are the same across multiple maps
    val keySeq = (ac.signatures.keySet ++ ac.availableToRedeem.keySet ++ ac.remainderAllocations.keySet).toSeq
      .zipWithIndex
    val keyMapping: Map[String, Int] = keySeq.toMap

    Bytes.concat(
      Ints.toByteArray(typeBytes.length),
      typeBytes,
      Longs.toByteArray(ac.fee),
      Longs.toByteArray(ac.timestamp),
      Ints.toByteArray(ac.signatures.size),
      Ints.toByteArray(ac.availableToRedeem.size),
      Ints.toByteArray(ac.remainderAllocations.size),
      Ints.toByteArray(keyMapping.size),
      ac.issuer.pubKeyBytes,
      keySeq.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(b._1.getBytes.length) ++ b._1.getBytes),
      ac.signatures.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(_.signature)
      ),
      ac.availableToRedeem.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(box => box._1.pubKeyBytes ++ Longs.toByteArray(box._2))
      ),
      ac.remainderAllocations.foldLeft(Array[Byte]())((a, b) => a ++ Ints.toByteArray(keyMapping(b._1)) ++
        Ints.toByteArray(b._2.length) ++ b._2.flatMap(alloc => alloc._1.pubKeyBytes ++ Longs.toByteArray(alloc._2))
      ),
      ac.data.getBytes,
      Ints.toByteArray(ac.data.getBytes.length)
    )
  }

  //noinspection ScalaStyle
  override def parseBytes(bytes: Array[Byte]): Try[AssetRedemption] = Try {
    val dataLen: Int = Ints.fromByteArray(bytes.slice(bytes.length - Ints.BYTES, bytes.length))
    val data: String = new String(
      bytes.slice(bytes.length - Ints.BYTES - dataLen, bytes.length - Ints.BYTES)
    )
    val typeLength = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLength))
    var numReadBytes = Ints.BYTES + typeLength
    val bytesWithoutType = bytes.slice(numReadBytes, bytes.length)


    val Array(fee: Long, timestamp: Long) = (0 until 2).map { i =>
      Longs.fromByteArray(bytesWithoutType.slice(i * Longs.BYTES, (i + 1) * Longs.BYTES))
    }.toArray

    numReadBytes = 2 * Longs.BYTES

    val Array(sigLength: Int, availableToRedeemLength: Int, amountsLength: Int, keyMappingSize: Int) = (0 until 4).map {
      i =>
        Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + i * Ints.BYTES, numReadBytes + (i + 1) * Ints.BYTES))
    }.toArray

    numReadBytes += 4 * Ints.BYTES

    val issuer = PublicKey25519Proposition(bytesWithoutType.slice(numReadBytes,
      numReadBytes + Constants25519.PubKeyLength))

    numReadBytes += Constants25519.PubKeyLength

    val keyMapping: Map[Int, String] = (0 until keyMappingSize).map { i =>
      val strLen = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES))
      val assetId = new String(bytesWithoutType.slice(numReadBytes + Ints.BYTES, numReadBytes + Ints.BYTES + strLen))

      numReadBytes += Ints.BYTES + strLen
      i -> assetId
    }.toMap

    val signatures: Map[String, IndexedSeq[Signature25519]] = (0 until sigLength).map { i =>

      val assetId: String = keyMapping(Ints.fromByteArray(
        bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)
      ))

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
      assetId -> sigs
    }.toMap

    val availableToRedeem: Map[String, IndexedSeq[(PublicKey25519Proposition, Nonce)]] = (0 until availableToRedeemLength)
      .map { _ =>
        var bytesSoFar = 0
        val assetId = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

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

        assetId -> boxes
      }.toMap

    val remainderAllocations: Map[String, IndexedSeq[(PublicKey25519Proposition, Long)]] = (0 until amountsLength).map {
      _ =>
        val assetId = keyMapping(Ints.fromByteArray(bytesWithoutType.slice(numReadBytes, numReadBytes + Ints.BYTES)))

        val allocationLength = Ints.fromByteArray(bytesWithoutType.slice(numReadBytes + Ints.BYTES,
          numReadBytes + 2 * Ints.BYTES))

        val startPosition = numReadBytes + 2 * Ints.BYTES
        val chunkSize = Constants25519.PubKeyLength + Longs.BYTES

        val allocationSeq: IndexedSeq[(PublicKey25519Proposition, Long)] = (0 until allocationLength).map { i =>
          val prop = PublicKey25519Proposition(
            bytesWithoutType.slice(startPosition + i * chunkSize,
              startPosition + i * chunkSize + Constants25519.PubKeyLength)
          )
          val amount = Longs.fromByteArray(
            bytesWithoutType.slice(startPosition + i * chunkSize + Constants25519.PubKeyLength,
              startPosition + (i + 1) * chunkSize)
          )
          prop -> amount
        }

        numReadBytes += 2 * Ints.BYTES + allocationLength * chunkSize
        assetId -> allocationSeq
    }.toMap

    AssetRedemption(availableToRedeem, remainderAllocations, signatures, issuer, fee, timestamp, data)
  }
}
