package co.topl.modifier.transaction

import co.topl.address.ToplAddress
import co.topl.crypto.{ProofOfKnowledge, ProofOfKnowledgeProposition, Secret}
import co.topl.nodeView.state.box.{Box, BoxId, PublicKeyNoncedBox, TokenBox}
import com.google.common.primitives.Longs
import scorex.crypto.hash.{Blake2b256, Digest32}

abstract class TransferTransaction[S <: Secret, P <: ProofOfKnowledgeProposition[S]]
(val from      : IndexedSeq[(ToplAddress[S], Box.Nonce)],
 val to        : IndexedSeq[(ToplAddress[S], TokenBox.Value)],
 val signatures: Map[P, ProofOfKnowledge[S, P]],
 val fee       : Long,
 val timestamp : Long,
 val data      : String
) extends Transaction {

  override val newBoxes: Traversable[TokenBox]

  lazy val boxIdsToOpen: IndexedSeq[BoxId] = from.map { case (prop, nonce) =>
    PublicKeyNoncedBox.idFromBox(prop, nonce)
  }

  lazy val hashNoNonces: Digest32 = Blake2b256(
    to.flatMap(_._1.pubKeyBytes).toArray ++
      //Longs.toByteArray(timestamp) ++
      Longs.toByteArray(fee) ++
      data.getBytes
  )

  //YT NOTE - removed timestamp and unlockers since that will be updated after signatures are received
  def commonMessageToSign: Array[Byte] =
    to.flatMap(_._1.pubKeyBytes).toArray ++
      newBoxes.foldLeft(Array[Byte]())((acc, x) => acc ++ x.bytes)

  Longs.toByteArray(fee) ++
    boxIdsToOpen.foldLeft(Array[Byte]())(_ ++ _.hashBytes) ++
    data.getBytes
}



