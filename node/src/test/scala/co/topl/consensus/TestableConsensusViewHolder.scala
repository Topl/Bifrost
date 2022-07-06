package co.topl.consensus

import akka.Done
import akka.actor.typed._
import akka.actor.typed.scaladsl.AskPattern._
import akka.pattern.StatusReply
import akka.util.Timeout
import co.topl.consensus.NxtConsensus.StateUpdate
import co.topl.modifier.block.Block

import scala.concurrent.Await
import scala.concurrent.duration._

object TestableConsensusViewHolder {
  implicit val timeout: Timeout = Timeout(10.seconds)

  def consensusViewOf(
    consensusViewHolder: ActorRef[NxtConsensus.ReceivableMessage]
  )(implicit system:     ActorSystem[_]): NxtConsensus.State =
    Await.result(
      consensusViewHolder.askWithStatus[NxtConsensus.State](NxtConsensus.ReceivableMessages.LookupState),
      10.seconds
    )

  def updateConsensusView(consensusViewHolder: ActorRef[NxtConsensus.ReceivableMessage], block: Block)(implicit
    system:                                    ActorSystem[_]
  ): Unit =
    Await.result(
      consensusViewHolder.ask[StatusReply[Done]](
        NxtConsensus.ReceivableMessages
          .UpdateState(block.id, StateUpdate(None, None), _)
      ),
      10.seconds
    )
}
