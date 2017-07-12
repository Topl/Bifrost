package bifrost.network

import akka.actor.{Actor, ActorRef}
import bifrost.BifrostNodeViewHolder
import bifrost.scorexMod.GenericNodeViewHolder._
import bifrost.scorexMod.GenericNodeViewSynchronizer.GetLocalSyncInfo
import bifrost.scorexMod.{GenericNodeViewHolder, GenericNodeViewSynchronizer}
import scorex.core.consensus.{History, SyncInfo}
import scorex.core.network.NetworkController.{DataFromPeer, SendToNetwork}
import scorex.core.network._
import scorex.core.network.message.BasicMsgDataTypes._
import scorex.core.network.message.{InvSpec, RequestModifierSpec, _}
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.Proposition
import scorex.core.utils.ScorexLogging
import scorex.core.{LocalInterface, NodeViewModifier}
import scorex.crypto.encode.Base58

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

/**
  * A middle layer between a node view holder(NodeViewHolder) and a network
  *
  * @param networkControllerRef
  * @param viewHolderRef
  * @param localInterfaceRef
  * @param syncInfoSpec
  */
class BifrostNodeViewSynchronizer(networkControllerRef: ActorRef,
                                  viewHolderRef: ActorRef,
                                  localInterfaceRef: ActorRef,
                                  syncInfoSpec: SyncInfoMessageSpec[SyncInfo])
  extends GenericNodeViewSynchronizer(networkControllerRef, viewHolderRef, localInterfaceRef, syncInfoSpec) {

  override def preStart(): Unit = {
    //register as a handler for some types of messages
    val messageSpecs = Seq(InvSpec, RequestModifierSpec, ModifiersSpec, syncInfoSpec, new ProducerNotifySpec)
    networkControllerRef ! NetworkController.RegisterMessagesHandler(messageSpecs, self)

    //subscribe for failed transaction,
    val events = Seq(
      GenericNodeViewHolder.EventType.FailedTransaction,
      GenericNodeViewHolder.EventType.FailedPersistentModifier,
      GenericNodeViewHolder.EventType.SuccessfulTransaction,
      GenericNodeViewHolder.EventType.SuccessfulPersistentModifier
    )
    viewHolderRef ! Subscribe(events)

    context.system.scheduler.schedule(2.seconds, 15.seconds)(self ! GetLocalSyncInfo)
  }

  private def processProposal: Receive = {
    case DataFromPeer(spec: ProducerNotifySpec, data: ProducerProposal, remote) =>
      networkControllerRef ! NetworkController.SendToNetwork(
        Message[ProducerProposal](spec, Right(data), None), BroadcastExceptOf(Seq(remote))
      )
  }

  private def newProposal: Receive = {
    case DataFromPeer(spec: ProducerNotifySpec, data: ProducerProposal, remote) =>
      networkControllerRef ! NetworkController.SendToNetwork(
        Message[ProducerProposal](spec, Right(data), None), BroadcastExceptOf(Seq(remote))
      )
  }

  override def receive: Receive = newProposal orElse processProposal orElse super.receive
}

