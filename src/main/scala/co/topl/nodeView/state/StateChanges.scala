package co.topl.nodeView.state

import co.topl.crypto.{ PrivateKey25519, ProofOfKnowledgeProposition }
import co.topl.modifier.block.Block
import co.topl.nodeView.state.box.{ Box, BoxId, PolyBox }
import com.google.common.primitives.Longs

import scala.util.Try

case class StateChanges( override val boxIdsToRemove: Set[BoxId],
                         override val toAppend: Set[Box],
                       ) extends GenericStateChanges[Any, ProofOfKnowledgeProposition[PrivateKey25519], Box](boxIdsToRemove, toAppend)

object StateChanges {
  type BX = Box
  type BPMOD = Block

  def apply(mod: BPMOD): Try[StateChanges] =
    Try {

      // extract the needed box data from all transactions within a block and
      // aggregate the transaction data into separate lists for updating state
      val (toRemove: Set[BoxId], toAdd: Set[BX], reward: Long) =
        mod.transactions.map {
          tx => (tx.boxIdsToOpen.toSet, tx.newBoxes.toSet, tx.fee)
        }.foldLeft((Set[BoxId](), Set[BX](), 0L))(
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
      new StateChanges(toRemove, finalToAdd)
    }
}
