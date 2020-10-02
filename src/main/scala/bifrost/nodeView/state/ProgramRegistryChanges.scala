package bifrost.nodeView.state

import bifrost.modifier.block.Block
import bifrost.modifier.transaction.{ ProgramCreation, ProgramMethodExecution, ProgramTransfer }
import bifrost.nodeView.box.ProgramBox

import scala.util.Try

case class ProgramRegistryChanges (toRemove: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                   toUpdate: Map[ProgramBoxRegistry.K, Seq[ProgramBoxRegistry.V]],
                                  )

object ProgramRegistryChanges {
  type K = ProgramBoxRegistry.K
  type V = ProgramBoxRegistry.V

  def apply(mod: Block): Try[ProgramRegistryChanges] =
    Try {

      def processToMap(boxSeq: Seq[ProgramBox]): Map[K, Seq[V]] = {
        boxSeq.groupBy(_.value).map { case (k,box) => (k, box.map(b => BoxId(b.id))) }
      }

      // extract the needed box data from all transactions within a block
      val (removeSeq: Seq[ProgramBox], updateSeq: Seq[ProgramBox])  =
        mod.transactions match {
          case Some(txSeq) =>
            txSeq.map({
              case tx: ProgramMethodExecution => (Seq(), tx.newBoxes.toSeq)
              case tx: ProgramCreation        => (Seq(), tx.newBoxes.toSeq)
              case tx: ProgramTransfer        => (Seq(), tx.newBoxes.toSeq)
              // case tx: ProgramDeletion     => (Some(???),None) // <-- this is the only case that should result in removing a program id
              case _                          => (Seq(), Seq()) // JAA - not sure if this is needed but added to be exhaustive
            }).foldLeft((Seq[ProgramBox](), Seq[ProgramBox]()))((acc, txData) => {
              (acc._1 ++ txData._1, acc._2 ++ txData._2)
            })

          case None => (Seq[ProgramBox](), Seq[ProgramBox]())
        }

      val toRemove = processToMap(removeSeq)
      val toUpdate = processToMap(updateSeq)

      // return the state changes that can be applied
      new ProgramRegistryChanges(toRemove, toUpdate)
    }
}
