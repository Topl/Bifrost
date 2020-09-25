package bifrost.state

import bifrost.modifier.block.Block
import bifrost.modifier.box.{Box, ProgramBox, PublicKeyNoncedBox, StateBox, TokenBox}
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.transaction.bifrostTransaction.{ProgramCreation, ProgramMethodExecution, ProgramTransaction, TransferTransaction}

import scala.util.Try

case class ProgramRegistryChanges (toUpdate: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                   toAppend: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                  ) extends TransactionAggregator

object ProgramRegistryChanges {
  type BX = Box
  type BPMOD = Block
  type K = ProgramBoxRegistry.K

  def apply(mod: BPMOD): Try[ProgramRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (updateSeq: Seq[(K, ProgramBox)], addSeq: Seq[(K, ProgramBox)])  =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: ProgramMethodExecution => (Some((tx.executionBox.stateBoxIds.head, tx.newBoxes.head)), None)
              case tx: ProgramCreation        => (None, Some((tx.newStateBoxes.head.value, tx.newStateBoxes.head)))
            }).foldLeft((Seq[(K, ProgramBox)](), Seq[(K, ProgramBox)]()))((acc, txData) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })
        }

      val toUpdate: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]] =
        updateSeq.groupBy(_._1).map { case (k,v) => (k, v.map(_._2.id)) }


      val toAppend: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]] =
        addSeq.groupBy(_._1).map { case (k,v) => (k, v.map(_._2.id)) }

      // return the state changes that can be applied
      new ProgramRegistryChanges(toUpdate, toAppend)
    }
}
