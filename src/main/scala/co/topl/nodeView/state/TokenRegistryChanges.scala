package co.topl.nodeView.state

import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ AssetCreation, CoinbaseTransaction, TransferTransaction }
import co.topl.nodeView.state.box.{ BoxId, PublicKeyNoncedBox, TokenBox }

import scala.util.Try

case class TokenRegistryChanges ( toRemove: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]],
                                  toAppend: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]],
                                )

object TokenRegistryChanges {
  type K = TokenBoxRegistry.K
  type V = TokenBoxRegistry.V

  def apply ( mod: Block ): Try[TokenRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (fromSeq: Seq[(K, Long)], toSeq: Seq[TokenBox]) =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: TransferTransaction => (tx.from, tx.newBoxes.toSeq)
              case tx: AssetCreation       => (Seq(), tx.newBoxes.toSeq)
              case tx: CoinbaseTransaction => (Seq(), tx.newBoxes.toSeq)
              case _                       => (Seq(), Seq()) // JAA - not sure if this is needed but added to be exhaustive
            }).foldLeft((Seq[(K, Long)](), Seq[TokenBox]()))(( acc, txData ) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })

          case None => (Seq[(K, Long)](), Seq[TokenBox]())
        }

      val toRemove: Map[K, Seq[V]] =
        fromSeq.groupBy(_._1).map { case (k, v) => (k, v.map(kv => BoxId(PublicKeyNoncedBox.idFromBox(k, kv._2)))) }

      val toAppend: Map[K, Seq[V]] =
        toSeq.groupBy(_.proposition).map { case (k, v) => (k, v.map(box => BoxId(box.id))) }

      // return the state changes that can be applied
      new TokenRegistryChanges(toRemove, toAppend)
    }
}

