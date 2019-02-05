package bifrost.network

import akka.actor.ActorRef
import bifrost.BifrostNodeViewHolder
import bifrost.history.{BifrostSyncInfo, BifrostSyncInfoMessageSpec}
import bifrost.scorexMod.GenericNodeViewHolder._
import bifrost.scorexMod.GenericNodeViewSynchronizer.GetLocalSyncInfo
import bifrost.scorexMod.{GenericNodeViewHolder, GenericNodeViewSynchronizer}
import bifrost.network.NetworkController.DataFromPeer
import bifrost.network._
import bifrost.network.message.{InvSpec, RequestModifierSpec, _}
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.proposition.ProofOfKnowledgeProposition
import bifrost.transaction.state.PrivateKey25519

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
                                  syncInfoSpec: BifrostSyncInfoMessageSpec.type)
  extends GenericNodeViewSynchronizer[ProofOfKnowledgeProposition[PrivateKey25519],
    BifrostTransaction,
    BifrostSyncInfo,
    BifrostSyncInfoMessageSpec.type
    ](networkControllerRef, viewHolderRef, localInterfaceRef, syncInfoSpec) {

  override def preStart(): Unit = {
    //register as a handler for some types of messages
    val messageSpecs = Seq(InvSpec, RequestModifierSpec, ModifiersSpec, syncInfoSpec)
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
}

