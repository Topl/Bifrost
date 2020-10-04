package co.topl.modifier.transaction

import co.topl.crypto.{ FastCryptographicHash, Signature25519 }
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{ PolyBox, PublicKeyNoncedBox }
import com.google.common.primitives.{ Ints, Longs }
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

abstract class ProgramTransaction extends Transaction {

  def owner: ProgramTransaction.O

  def signatures: ProgramTransaction.SIG

  def preFeeBoxes: ProgramTransaction.FBX

  def fees: ProgramTransaction.F

  override val fee: Long = fees.values.sum

  lazy val feeBoxIdKeyPairs: IndexedSeq[(Array[Byte], PublicKey25519Proposition)] = preFeeBoxes.toIndexedSeq
    .flatMap {
      case (prop, v) =>
        v.map {
          case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
        }
    }

  lazy val commonJson: Json = Map(
    "txHash" -> id.toString.asJson,
    "owner" -> Base58.encode(owner.pubKeyBytes).asJson,
    "signatures" -> signatures.map { case (prop, sig) => Base58.encode(prop.pubKeyBytes) -> Base58.encode(sig.bytes)
      .asJson
    }.asJson,
    "feePreBoxes" -> preFeeBoxes.map { case (prop: PublicKey25519Proposition, preBoxes: IndexedSeq[(Nonce, Long)]) =>
      Base58.encode(prop.pubKeyBytes) -> preBoxes.map { pb =>
        Map(
          "nonce" -> pb._1.toString.asJson,
          "value" -> pb._2.toString.asJson
          ).asJson
      }
    }.asJson,
    "fees" -> fees.map { case (prop, amount) => Base58.encode(prop.pubKeyBytes) -> amount.asJson }.asJson,
    "timestamp" -> timestamp.asJson
  ).asJson

  def deductedFeeBoxes(hashNoNonces: Array[Byte]): IndexedSeq[PolyBox] = {
    val canSend = preFeeBoxes.mapValues(_.map(_._2).sum)
    val preboxesLessFees: IndexedSeq[(PublicKey25519Proposition, Long)] = canSend
      .toIndexedSeq
      .map { case (prop, amount) => prop -> (amount - fees(prop)) }

    preboxesLessFees.zipWithIndex
      .map {
        case ((prop, value), idx) =>
          val nonce = ProgramTransaction
            .nonceFromDigest(
              FastCryptographicHash("ProgramCreation".getBytes
                ++ prop.pubKeyBytes
                ++ hashNoNonces
                ++ Ints.toByteArray(idx)))

          PolyBox(prop, nonce, value)
      }
  }
}




object ProgramTransaction {
  type O = PublicKey25519Proposition
  type SIG = Map[PublicKey25519Proposition, Signature25519]
  type FBX = Map[PublicKey25519Proposition, IndexedSeq[(Nonce, Long)]]
  type F = Map[PublicKey25519Proposition, Long]
  type RP = Map[String, String]

  def nonceFromDigest(digest: Array[Byte]): Nonce = Longs.fromByteArray(digest.take(8))

  def commonValidation(tx: ProgramTransaction): Try[Unit] = Try {

    /* Check for no-overflow and non-negativity of fees*/
    tx.fees.values.foreach(v => require(v >= 0, "There was a negative fee"))
    require(tx.fees.values.sum >= 0, "Fees did not sum to a positive value")

    /* Check for no-overflow and non-negativity of polys */
    require(tx.preFeeBoxes.forall { case (prop, preBoxes) =>
      preBoxes.map(_._2).sum >= 0 && preBoxes.forall(_._2 >= 0)
    },
      "There were negative polys provided or the sum was negative"
    )

    /* Check that fee is covered */
    require(tx.preFeeBoxes.forall { case (prop, preBoxes) => tx.fees.get(prop) match {
      case Some(fee) => preBoxes.map(_._2).sum >= fee
      case None => false
    }
    },
      "There was an insufficient amount of polys provided to cover the fees"
    )

    require(tx.timestamp >= 0, "The timestamp was invalid")
  }

  def commonDecode(rawOwner: String,
                   rawSignatures: RP,
                   rawFeeBoxes: Map[String, IndexedSeq[(Long, Long)]],
                   rawFees: Map[String, Long]): (O, SIG, FBX, F) = {
    val owner = Transaction.stringToPubKey(rawOwner)
    val signatures = rawSignatures.map { case (key, value) =>
      if (value == "") (Transaction.stringToPubKey(key), Signature25519(Array.empty[Byte]))
      else (Transaction.stringToPubKey(key), Transaction.stringToSignature(value))
    }
    val preFeeBoxes = rawFeeBoxes.map { case (key, value) => (Transaction.stringToPubKey(key), value) }
    val fees = rawFees.map { case (key, value) => (Transaction.stringToPubKey(key), value) }
    (owner, signatures, preFeeBoxes, fees)
  }
}