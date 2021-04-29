//accumulators co.topl.modifier.transaction
//
//import co.topl.attestation.proof.SignatureCurve25519
//import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
//import co.topl.modifier.box.{ BoxId, PolyBox }
//import com.google.common.primitives.Ints
//import co.topl.crypto.hash.Blake2b256
//
//import scala.util.Try
//
//abstract class ProgramTransaction extends Transaction {
//
//  def owner: ProgramTransaction.O
//
//  def signatures: ProgramTransaction.SIG
//
////  def preFeeBoxes: ProgramTransaction.FBX
//
////  def fees: ProgramTransaction.F
//
//  override val fee: Long = fees.values.sum
//
////  lazy val feeBoxIdKeyPairs: IndexedSeq[(BoxId, PublicKeyCurve25519Proposition)] = preFeeBoxes.toIndexedSeq
////    .flatMap {
////      case (prop, v) =>
////        v.map {
////          case (nonce, _) => (PublicKeyNoncedBox.idFromBox(prop, nonce), prop)
////        }
////    }
//
////  def deductedFeeBoxes(hashNoNonces: Array[Byte]): IndexedSeq[PolyBox] = {
////    val canSend = preFeeBoxes.mapValues(_.map(_._2).sum)
////    val preboxesLessFees: IndexedSeq[(PublicKeyCurve25519Proposition, Long)] = canSend
////      .toIndexedSeq
////      .map { case (prop, amount) => prop -> (amount - fees(prop)) }
////
////    preboxesLessFees.zipWithIndex
////      .map {
////        case ((prop, value), idx) =>
////          val nonce = Transaction
////            .nonceFromDigest(
////              Blake2b256("ProgramCreation".getBytes
////                ++ prop.pubKeyBytes
////                ++ hashNoNonces
////                ++ Ints.toByteArray(idx)))
////
////          PolyBox(prop, nonce, value)
////      }
////  }
//}
//
//
//
//
//object ProgramTransaction {
//  type O = PublicKeyCurve25519Proposition
//  type SIG = Map[PublicKeyCurve25519Proposition, SignatureCurve25519]
//  type FBX = Map[PublicKeyCurve25519Proposition, IndexedSeq[(Nonce, Long)]]
//  type F = Map[PublicKeyCurve25519Proposition, Long]
//  type RP = Map[String, String]
//
//  def syntacticValidate ( tx: ProgramTransaction): Try[Unit] = Try {
//
//    /* Check for no-overflow and non-negativity of fees*/
//    tx.fees.values.foreach(v => require(v >= 0, "There was a negative fee"))
//    require(tx.fees.values.sum >= 0, "Fees did not sum to a positive value")
//
//    /* Check for no-overflow and non-negativity of polys */
//    require(tx.preFeeBoxes.forall { case (prop, preBoxes) =>
//      preBoxes.map(_._2).sum >= 0 && preBoxes.forall(_._2 >= 0)
//    },
//      "There were negative polys provided or the sum was negative"
//    )
//
//    /* Check that fee is covered */
//    require(tx.preFeeBoxes.forall { case (prop, preBoxes) => tx.fees.get(prop) match {
//      case Some(fee) => preBoxes.map(_._2).sum >= fee
//      case None => false
//    }
//    },
//      "There was an insufficient amount of polys provided to cover the fees"
//    )
//
//    require(tx.timestamp >= 0, "The timestamp was invalid")
//  }
//}
