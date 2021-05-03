package co.topl.nodeView.state

import co.topl.attestation.Address
import co.topl.modifier.block.Block
import co.topl.modifier.box.{Box, TokenValueHolder}
import co.topl.modifier.transaction.TransferTransaction
import co.topl.utils.NetworkType.NetworkPrefix

import scala.util.Try

case class TokenRegistryChanges(
  toRemove: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]],
  toAppend: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]]
)

object TokenRegistryChanges {
  type K = TokenBoxRegistry.K
  type V = TokenBoxRegistry.V

  def apply(mod: Block)(implicit networkPrefix: NetworkPrefix): Try[TokenRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (fromSeq, toSeq) =
        mod.transactions
          .map {
            case tx: TransferTransaction[_, _] => (tx.from, tx.newBoxes)
            case _                             => (Seq(), Seq()) // JAA - not sure if this is needed but added to be exhaustive
          }
          .foldLeft((Seq[(K, Box.Nonce)](), Seq[Box[TokenValueHolder]]())) { (acc, txData) =>
            (acc._1 ++ txData._1, acc._2 ++ txData._2)
          }

      val toRemove: Map[K, Seq[V]] =
        fromSeq.groupBy(_._1).map { case (k, v) => (k, v.map { case (_, nonce) => nonce }) }

      val toAppend: Map[K, Seq[V]] =
        toSeq.groupBy(bx => Address(bx.evidence)).map { case (k, v) => (k, v.map(_.nonce)) }

      // return the state changes that can be applied
      new TokenRegistryChanges(toRemove, toAppend)
    }
}
