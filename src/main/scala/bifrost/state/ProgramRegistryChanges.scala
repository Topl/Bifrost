package bifrost.state

import bifrost.modifier.block.Block
import bifrost.modifier.box.{ Box, ProgramBox }
import bifrost.modifier.transaction.bifrostTransaction.{ ProgramCreation, ProgramMethodExecution, ProgramTransfer }

import scala.util.Try

case class ProgramRegistryChanges (toRemove: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                   toUpdate: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                  )

object ProgramRegistryChanges {
  type BX = Box
  type BPMOD = Block
  type K = ProgramBoxRegistry.K
  type V = ProgramBoxRegistry.V

  def apply(mod: BPMOD): Try[ProgramRegistryChanges] =
    Try {

      // extract the needed box data from all transactions within a block
      val (removeSeq: Seq[(K, ProgramBox)], updateSeq: Seq[(K, ProgramBox)])  =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: ProgramMethodExecution => (None, Some((tx.executionBox.stateBoxIds.head, tx.newBoxes.head)))
              case tx: ProgramCreation        => (None, Some((tx.newStateBoxes.head.value, tx.newStateBoxes.head)))
              case tx: ProgramTransfer        => ???
              case _                          => (None, None) // JAA - not sure if this is needed but added to be exhaustive
            }).foldLeft((Seq[(K, ProgramBox)](), Seq[(K, ProgramBox)]()))((acc, txData) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })

          case None => (Seq[(K, ProgramBox)](), Seq[(K, ProgramBox)]())
        }

      val toRemove: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]] =
        removeSeq.groupBy(_._1).map { case (k,kv) => (k, kv.map(v => BoxId(v._2.id))) }

      val toUpdate: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]] =
        updateSeq.groupBy(_._1).map { case (k,kv) => (k, kv.map(v => BoxId(v._2.id))) }

      // return the state changes that can be applied
      new ProgramRegistryChanges(toRemove, toUpdate)
    }
}
