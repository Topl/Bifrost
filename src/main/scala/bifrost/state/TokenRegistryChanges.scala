package bifrost.state

import bifrost.modifier.block.Block
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{ Box, PublicKeyNoncedBox, TokenBox }
import bifrost.modifier.transaction.bifrostTransaction.{ AssetCreation, CoinbaseTransaction, TransferTransaction }

import scala.util.Try

case class TokenRegistryChanges ( toRemove: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]],
                                  toAppend: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]],
                                )

object TokenRegistryChanges {
  type BX = Box
  type BPMOD = Block
  type PK = PublicKey25519Proposition

  def apply ( mod: BPMOD ): Try[TokenRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (fromSeq: Seq[(PK, Long)], toSeq: Seq[TokenBox]) =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: TransferTransaction => (tx.from, tx.newBoxes.toSet)
              case tx: AssetCreation       => ???
              case tx: CoinbaseTransaction => ???
              case _                       => (Seq(), Seq()) // JAA - not sure if this is needed but added to be exhaustive
            }).foldLeft((Seq[(PK, Long)](), Seq[TokenBox]()))(( acc, txData ) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })

          case None => (Seq[(PK, Long)](), Seq[TokenBox]())
        }

      val toRemove: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]] =
        fromSeq.groupBy(_._1).map { case (k, v) => (k, v.map(kv => BoxId(PublicKeyNoncedBox.idFromBox(k, kv._2)))) }

      val toAppend: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]] =
        toSeq.groupBy(_.proposition).map { case (k, v) => (k, v.map(box => BoxId(box.id))) }

      // return the state changes that can be applied
      new TokenRegistryChanges(toRemove, toAppend)
    }
}

