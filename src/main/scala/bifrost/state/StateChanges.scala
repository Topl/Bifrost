package bifrost.state

import bifrost.crypto.PrivateKey25519
import bifrost.modifier.block.Block
import bifrost.modifier.box.{Box, PolyBox}
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import com.google.common.primitives.Longs

import scala.util.Try

case class StateChanges( override val boxIdsToRemove: Set[Array[Byte]],
                         override val toAppend: Set[Box],
                         timestamp: Long
                       ) extends GenericStateChanges[Any, ProofOfKnowledgeProposition[PrivateKey25519], Box](boxIdsToRemove, toAppend)

object StateChanges {
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = Box
  type BPMOD = Block
  type GSC = GenericStateChanges[Any, P, BX]


  //todo - byte array set quality is incorrectly overloaded (shallow not deep), consider using ByteArrayWrapper instead
  //todo - LSMStore will throw error if given duplicate keys in toRemove or toAppend so this needs to be fixed
  def apply(mod: BPMOD): Try[GSC] =
    Try {

      val boxDeltas: Seq[(Set[Array[Byte]], Set[BX], Long)] =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map(tx => (tx.boxIdsToOpen.toSet, tx.newBoxes.toSet, tx.fee))
          case _ => Seq((Set[Array[Byte]](), Set[BX](), 0L))
        }

      val (toRemove: Set[Array[Byte]], toAdd: Set[BX], reward: Long) =
        boxDeltas.foldLeft((Set[Array[Byte]](), Set[BX](), 0L))(
          (aggregate, boxDelta) => {
            (
              aggregate._1 ++ boxDelta._1,
              aggregate._2 ++ boxDelta._2,
              aggregate._3 + boxDelta._3
            )
          }
        )

      // compute the fees to be transferred to the validator
      val finalToAdd =
        if (reward != 0) {
          val gen = mod.forgerBox.proposition
          val rewardNonce = Longs.fromByteArray(mod.id.hashBytes.take(Longs.BYTES))
          toAdd + PolyBox(gen, rewardNonce, reward)
        }
        else toAdd

      // return the state changes that can be applied
      new StateChanges(toRemove, finalToAdd, mod.timestamp)
    }
}
