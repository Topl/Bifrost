package bifrost.state

import bifrost.modifier.block.Block
import bifrost.modifier.box.{ Box, PublicKeyNoncedBox, TokenBox }
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.{ ProgramMethodExecution, ProgramTransaction, TransferTransaction }

import scala.util.Try

case class ProgramRegistryChanges (toRemove: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                   toAppend: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                  ) extends TransactionAggregator

object ProgramRegistryChanges {
  type BX = Box
  type BPMOD = Block
  type PK = PublicKey25519Proposition

  def apply(mod: BPMOD): Try[ProgramRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (fromSeq: Seq[(PK, Long)], toSeq: Seq[TokenBox])  =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: ProgramMethodExecution => (tx.executionBox.stateBoxIds, tx.newBoxes.head)
            }).foldLeft((Seq[(PK, Long)](), Seq[Box]()))((acc, txData) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })
        }

      val toRemove: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]] =
        fromSeq.groupBy(_._1).map { case (k,v) => (k, v.map(kv => PublicKeyNoncedBox.idFromBox(k, kv._2))) }

      val toAppend: Map[TokenBoxRegistry.K, Seq[TokenBoxRegistry.V]] =
        toSeq.groupBy(_.proposition).map { case (k,v) => (k, v.map(_.id)) }

      // return the state changes that can be applied
      new TokenRegistryChanges(toRemove, toAppend)
    }
}
