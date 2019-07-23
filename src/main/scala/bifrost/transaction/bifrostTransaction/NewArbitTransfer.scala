//package bifrost.transaction.bifrostTransaction
//
//
//import java.time.Instant
//
//import bifrost.crypto.hash.FastCryptographicHash
//import BifrostTransaction.{Nonce, Value}
//import bifrost.NodeViewModifier.ModifierId
//import bifrost.bfr.BFR
//import bifrost.scorexMod.GenericBoxTransaction
//import bifrost.transaction.account.PublicKeyNoncedBox
//import bifrost.transaction.box.proposition.{ProofOfKnowledgeProposition, PublicKey25519Proposition}
//import bifrost.transaction.box.{ArbitBox, BifrostBox, BoxUnlocker}
//import bifrost.transaction.proof.Signature25519
//import bifrost.transaction.state.PrivateKey25519
//import bifrost.wallet.BWallet
//import com.google.common.primitives.{Ints, Longs}
//import io.circe.Json
//import io.circe.syntax._
//import scorex.crypto.encode.Base58
//
//import scala.util.Try
//
//case class NewArbitTransfer( val from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
//                             val to: IndexedSeq[(PublicKey25519Proposition, Long)],
//                             val signatures: IndexedSeq[(PublicKey25519Proposition, Signature25519)],
//                             val fee: Long,
//                             val timestamp: Long,
//                             val data: String)
//  //extends TransferTransaction(from, to, signatures, fee, timestamp, data)
////extends BifrostTransaction
//  //extends GenericBoxTransaction[ProofOfKnowledgeProposition[PrivateKey25519], Any, BifrostBox]
//{
//
//  println("<<<<<<<<<<<<<<<<<<")
//  println("Entered")
//
//  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, nonce) =>
//    PublicKeyNoncedBox.idFromBox(prop, nonce)
//  }
////  lazy val boxIdsToOpen: IndexedSeq[Array[Byte]] = from.map { case (prop, boxId) =>
////    boxId
////  }
////
////  override lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = boxIdsToOpen.zip(signatures).map {
////    case (boxId, signature) =>
////      new BoxUnlocker[PublicKey25519Proposition] {
////        override val closedBoxId: Array[Byte] = boxId
////        override val boxKey: Signature25519 = signature
////      }
////  }
//
//
//   lazy val unlockers: Traversable[BoxUnlocker[PublicKey25519Proposition]] = from.map {
//    case (prop, nonce) =>
//      new BoxUnlocker[PublicKey25519Proposition] {
//        override val closedBoxId: Array[Byte] = PublicKeyNoncedBox.idFromBox(prop, nonce)
//        //TODO improve signature access
//        //override val boxKey: Signature25519 = signatures.filter(_._1.pubKeyBytes sameElements(prop.pubKeyBytes)).map(_._2).headOption.getOrElse(Signature25519(Array()))
//      override val boxKey: Signature25519 = Signature25519(Array())
//      }
//  }
//
//  lazy val hashNoNonces = FastCryptographicHash(
//    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
//     // unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
//      Longs.toByteArray(timestamp) ++
//      Longs.toByteArray(fee)
//  )
//
////  type M = NewArbitTransfer
////
////   lazy val serializer = NewArbitTransferCompanion
//
//   override def toString: String = s"NewArbitTransfer(${json.noSpaces})"
//
//    lazy val newBoxes: Traversable[BifrostBox] = to
//    .filter(toInstance => toInstance._2 > 0L)
//    .zipWithIndex
//    .map {
//      case ((prop, value), idx) =>
//        val nonce = ArbitTransfer
//          .nonceFromDigest(FastCryptographicHash("NewArbitTransfer".getBytes
//            ++ prop.pubKeyBytes
//            ++ hashNoNonces
//            ++ Ints.toByteArray(idx)))
//
//        ArbitBox(prop, nonce, value)
//    }
//
//  lazy val messageToSign: Array[Byte] =
//    "NewArbitTransfer".getBytes() ++
//      // newBoxes.map(_.bytes).reduce(_ ++ _) ++
//      // unlockers.map(_.closedBoxId).reduce(_ ++ _) ++
//      Longs.toByteArray(timestamp) ++
//      Longs.toByteArray(fee)
//  data.getBytes
//
//  lazy val id: ModifierId = FastCryptographicHash(messageToSign)
//
//   lazy val json: Json = Map(
//    "txHash" -> Base58.encode(id).asJson,
//    "txType" -> "NewArbitTransfer".asJson,
//    "newBoxes" -> newBoxes.map(b => Base58.encode(b.id).asJson).asJson,
//    "boxesToRemove" -> boxIdsToOpen.map(id => Base58.encode(id).asJson).asJson,
//    "from" -> from.map { s =>
//      Map(
//        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
//        "nonce" -> s._2.toString.asJson
//      ).asJson
//    }.asJson,
//    "to" -> to.map { s =>
//      Map(
//        "proposition" -> Base58.encode(s._1.pubKeyBytes).asJson,
//        "value" -> s._2.toString.asJson
//      ).asJson
//    }.asJson,
//    "signatures" -> signatures
//      .map(s => Base58.encode(s._2.signature).asJson)
//      .asJson,
//    "fee" -> fee.asJson,
//    "timestamp" -> timestamp.asJson,
//    "data" -> data.asJson,
//     "messageToSign" -> Base58.encode(messageToSign).asJson
//  ).asJson
//
//  println("Finished")
//  println(">>>>>>>>>>>>>>>>>>>>>>>>>>")
//}
//
////noinspection ScalaStyle
//object NewArbitTransfer extends NewTransferUtil {
//
//  def apply(from: IndexedSeq[(PublicKey25519Proposition, Nonce)],
//            to: IndexedSeq[(PublicKey25519Proposition, Value)],
//            signatures: IndexedSeq[(PublicKey25519Proposition, Signature25519)],
//            fee: Long,
//            timestamp: Long,
//            data: String): NewArbitTransfer = {
////    println()
////    println("Finished static method")
////    val params = parametersForApply(from, to, fee, timestamp, "ArbitTransfer", data).get
//    val res = NewArbitTransfer(from, to, signatures, fee, timestamp, data)
//    println(res.messageToSign)
//    println(res.boxIdsToOpen.length)
//    println(res.id)
//    println(res.json)
//    res
//  }
//
//
//  def createPrototype(bfr: BFR, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], sender: PublicKey25519Proposition, fee: Long, data: String): Try[NewArbitTransfer] = Try
//  {
//    println()
//    println("Entered create prototype")
//    val params = parametersForCreate(bfr, toReceive, sender, fee, "ArbitTransfer")
//    val timestamp = Instant.now.toEpochMilli
//    println()
//    println(params._1.length)
//    println(params._2.length)
//    println()
//    NewArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, IndexedSeq(), fee, timestamp, data)
//  }
//
//  def createFinal(bfr: BFR, toReceive: IndexedSeq[(PublicKey25519Proposition, Long)], sender: PublicKey25519Proposition, signatures: IndexedSeq[(PublicKey25519Proposition, Signature25519)], fee: Long, data: String): Try[NewArbitTransfer] = Try
//  {
//    val params = parametersForCreate(bfr, toReceive, sender, fee, "ArbitTransfer")
//    val timestamp = Instant.now.toEpochMilli
//    NewArbitTransfer(params._1.map(t => t._1 -> t._2), params._2, signatures, fee, timestamp, data)
//  }
//
//
//
////  def validate(tx: NewArbitTransfer): Try[Unit] = Try {
////    require(tx.from.size == tx.signatures.size)
////    require(tx.to.forall(_._2 >= 0L))
////    require(tx.fee >= 0)
////    require(tx.timestamp >= 0)
////    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
////      proof.isValid(prop, tx.messageToSign)
////    })
////  }
//}
//
