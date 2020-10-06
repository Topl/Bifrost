package co.topl.modifier.transaction

import co.topl.crypto.{ FastCryptographicHash, Signature25519 }
import co.topl.modifier.transaction.Transaction.Nonce
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{ BoxId, PublicKeyNoncedBox, TokenBox }
import com.google.common.primitives.Longs

abstract class TransferTransaction (val from      : IndexedSeq[(PublicKey25519Proposition, Nonce)],
                                    val to        : IndexedSeq[(PublicKey25519Proposition, Long)],
                                    val signatures: Map[PublicKey25519Proposition, Signature25519],
                                    val fee       : Long,
                                    val timestamp : Long,
                                    val data      : String
                                   ) extends Transaction {

  override val newBoxes: Traversable[TokenBox]

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  lazy val hashNoNonces: Array[Byte] = FastCryptographicHash(
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      //Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      data.getBytes
  )

  //YT NOTE - removed timestamp and unlockers since that will be updated after signatures are received
  def commonMessageToSign: Array[Byte] =
    to.map(_._1.pubKeyBytes).reduce(_ ++ _) ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)

  Longs.toByteArray(fee) ++
    boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _.hashBytes) ++
    data.getBytes
}



