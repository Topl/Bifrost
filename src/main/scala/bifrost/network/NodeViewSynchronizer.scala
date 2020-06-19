package bifrost.network


import java.net.InetSocketAddress

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.history.HistoryReader
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.{GetNodeViewChanges, ModifiersFromRemote, TransactionsFromRemote}
import bifrost.mempool.MemPoolReader
import bifrost.network.ModifiersStatus.Requested
import bifrost.network.NetworkController.ReceivableMessages.{PenalizePeer, RegisterMessageSpecs, SendToNetwork}
import bifrost.network.SharedNetworkMessages.ReceivableMessages.DataFromPeer
import bifrost.network.NodeViewSynchronizer.ReceivableMessages._
import bifrost.network.message._
import bifrost.network.peer.PenaltyType
import bifrost.nodeView.GenericNodeViewHolder.DownloadRequest
import bifrost.nodeView.NodeViewModifier
import bifrost.nodeView.NodeViewModifier.{idsToString, ModifierTypeId}
import bifrost.nodeView.PersistentNodeViewModifier
import bifrost.history.GenericHistory.{Fork, HistoryComparisonResult, Nonsense, Unknown, Younger}
import bifrost.modifier.ModifierId
import bifrost.utils.Logging
import bifrost.settings.NetworkSettings
import bifrost.state.StateReader
import bifrost.wallet.VaultReader
import bifrost.utils.NetworkTimeProvider
import bifrost.utils.BifrostEncoding
import bifrost.utils.serialization.BifrostSerializer
import bifrost.utils.MalformedModifierError

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

/**
  * A component which is synchronizing local node view (locked inside NodeViewHolder) with the p2p network.
  *
  * @param networkControllerRef reference to network controller actor
  * @param viewHolderRef        reference to node view holder actor
  * @param syncInfoSpec         SyncInfo specification
  * @tparam TX  transaction
  * @tparam SIS SyncInfoMessage specification
  */
class NodeViewSynchronizer[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MemPoolReader[TX] : ClassTag]
  (networkControllerRef: ActorRef,
     viewHolderRef: ActorRef,
     syncInfoSpec: SIS,
     networkSettings: NetworkSettings,
     timeProvider: NetworkTimeProvider,
     modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]])(implicit ec: ExecutionContext) extends Actor
      with Logging with BifrostEncoding {

  protected val deliveryTimeout: FiniteDuration = networkSettings.deliveryTimeout
  protected val maxDeliveryChecks: Int = networkSettings.maxDeliveryChecks
  protected val invSpec = new InvSpec(networkSettings.maxInvObjects)
  protected val requestModifierSpec = new RequestModifierSpec(networkSettings.maxInvObjects)
  protected val modifiersSpec = new ModifiersSpec(networkSettings.maxPacketSize)

  protected val deliveryTracker = new DeliveryTracker(context.system, deliveryTimeout, maxDeliveryChecks, self)
  protected val statusTracker = new SyncTracker(self, context, networkSettings, timeProvider)

  protected var historyReaderOpt: Option[HR] = None
  protected var mempoolReaderOpt: Option[MR] = None

  override def preStart(): Unit = {
    //register as a handler for synchronization-specific types of messages
    val messageSpecs: Seq[MessageSpec[_]] = Seq(invSpec, requestModifierSpec, modifiersSpec, syncInfoSpec)
    networkControllerRef ! RegisterMessageSpecs(messageSpecs, self)

    //register as a listener for peers got connected (handshaked) or disconnected
    context.system.eventStream.subscribe(self, classOf[HandshakedPeer])
    context.system.eventStream.subscribe(self, classOf[DisconnectedPeer])

    //subscribe for all the node view holder events involving modifiers and transactions
    context.system.eventStream.subscribe(self, classOf[ChangedHistory[HR]])
    context.system.eventStream.subscribe(self, classOf[ChangedMempool[MR]])
    context.system.eventStream.subscribe(self, classOf[ModificationOutcome])
    context.system.eventStream.subscribe(self, classOf[DownloadRequest])
    context.system.eventStream.subscribe(self, classOf[ModifiersProcessingResult[PMOD]])
    viewHolderRef ! GetNodeViewChanges(history = true, state = false, vault = false, mempool = true)

    statusTracker.scheduleSendSyncInfo()
  }

////////////////////////////////////////////////////////////////////////////////////
////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    processDataFromPeer orElse
      processSyncStatus orElse
      manageModifiers orElse
      viewHolderEvents orElse
      peerManagerEvents orElse
      nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  protected def processDataFromPeer: Receive = {

    // sync info is coming from another node
    case DataFromPeer(spec, syncInfo: SI@unchecked, remote)
      if spec.messageCode == syncInfoSpec.messageCode =>

      historyReaderOpt match {
        case Some(historyReader) =>
          val ext = historyReader.continuationIds(syncInfo, networkSettings.desiredInvObjects)
          val comparison = historyReader.compare(syncInfo)
          log.debug(s"Comparison with $remote having starting points ${idsToString(syncInfo.startingPoints)}. " +
            s"Comparison result is $comparison. Sending extension of length ${ext.length}")
          log.debug(s"Extension ids: ${idsToString(ext)}")

          if (!(ext.nonEmpty || comparison != Younger))
            log.warn("Extension is empty while comparison is younger")

          self ! OtherNodeSyncingStatus(remote, comparison, ext)
        case _ =>
      }

    // Object ids coming from other node.
    case DataFromPeer(spec, invData: InvData@unchecked, peer)
      if spec.messageCode == InvSpec.MessageCode =>

      (mempoolReaderOpt, historyReaderOpt) match {
        // Filter out modifier ids that are already in process (requested, received or applied)
        case (Some(mempool), Some(history)) =>
          val modifierTypeId = invData.typeId
          val newModifierIds = modifierTypeId match {
            case Transaction.modifierTypeId =>
              invData.ids.filter(mid => deliveryTracker.status(mid, mempool) == ModifiersStatus.Unknown)
            case _ =>
              invData.ids.filter(mid => deliveryTracker.status(mid, history) == ModifiersStatus.Unknown)
          }

          // request unknown ids from peer and set this ids to requested state.
          if (newModifierIds.nonEmpty) {
            val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, newModifierIds)), None)
            peer.handlerRef ! msg
            deliveryTracker.setRequested(newModifierIds, modifierTypeId, Some(peer))
          }

        case _ =>
          log.warn(s"Got data from peer while readers are not ready ${(mempoolReaderOpt, historyReaderOpt)}")
      }


    // other node asking for objects by their ids
    case DataFromPeer(spec, invData: InvData@unchecked, remote)
      if spec.messageCode == RequestModifierSpec.MessageCode =>

      readersOpt.foreach { readers =>
        val objs: Seq[NodeViewModifier] = invData.typeId match {
          case typeId: ModifierTypeId if typeId == Transaction.modifierTypeId =>
            readers._2.getAll(invData.ids)
          case _: ModifierTypeId =>
            invData.ids.flatMap(id => readers._1.modifierById(id))
        }

        log.debug(s"Requested ${invData.ids.length} modifiers ${idsToString(invData)}, " +
          s"sending ${objs.length} modifiers ${idsToString(invData.typeId, objs.map(_.id))} ")
        self ! ResponseFromLocal(remote, invData.typeId, objs)
      }

    // process modifiers received from another peer
    case DataFromPeer(spec, data: ModifiersData@unchecked, remote)
      if spec.messageCode == ModifiersSpec.MessageCode =>

      val typeId = data.typeId
      val modifiers = data.modifiers
      log.info(s"Got ${modifiers.size} modifiers of type $typeId from remote connected peer: $remote")
      log.trace(s"Received modifier ids ${modifiers.keySet.map(encoder.encodeId).mkString(",")}")

      // filter out non-requested modifiers
      val requestedModifiers = processSpam(remote, typeId, modifiers)

      modifierSerializers.get(typeId) match {
        case Some(serializer: BifrostSerializer[TX]@unchecked) if typeId == Transaction.modifierTypeId =>
          // parse all transactions and send them to node view holder
          val parsed: Iterable[TX] = parseModifiers(requestedModifiers, serializer, remote)
          viewHolderRef ! TransactionsFromRemote(parsed)

        case Some(serializer: BifrostSerializer[PMOD]@unchecked) =>
          // parse all modifiers and put them to modifiers cache
          val parsed: Iterable[PMOD] = parseModifiers(requestedModifiers, serializer, remote)
          val valid: Iterable[PMOD] = parsed.filter(validateAndSetStatus(remote, _))
          if (valid.nonEmpty) viewHolderRef ! ModifiersFromRemote[PMOD](valid)

        case _ =>
          log.error(s"Undefined serializer for modifier of type $typeId")
      }
  }

  protected def processSyncStatus: Receive = {

    // send local sync status to a peer
    case SendLocalSyncInfo =>
      historyReaderOpt.foreach(sendSync(statusTracker, _))

    // receive a sync status from a peer
    case OtherNodeSyncingStatus(remote, status, ext) =>
      statusTracker.updateStatus(remote, status)

      status match {
        case Unknown =>
          //todo: should we ban peer if its status is unknown after getting info from it?
          log.warn("Peer status is still unknown")
        case Nonsense =>
          log.warn("Got nonsense")
        case Younger | Fork =>
          sendExtension(remote, status, ext)
        case _ => // does nothing for `Equal` and `Older`
      }
  }

  protected def manageModifiers: Receive = {

    // Request data from a remote node
    case DownloadRequest(modifierTypeId: ModifierTypeId, modifierId: ModifierId) =>
      if (deliveryTracker.status(modifierId, historyReaderOpt.toSeq) == ModifiersStatus.Unknown) {
        requestDownload(modifierTypeId, Seq(modifierId))
      }

    // Respond with data from the local node
    case ResponseFromLocal(peer, _, modifiers: Seq[NodeViewModifier]) =>
      modifiers.headOption.foreach { head =>
        val modType = head.modifierTypeId

        @tailrec
        def sendByParts(mods: Seq[(ModifierId, Array[Byte])]): Unit = {
          var size = 5 //message type id + message size
          val batch = mods.takeWhile { case (_, modBytes) =>
            size += NodeViewModifier.ModifierIdSize + 4 + modBytes.length
            size < networkSettings.maxPacketSize
          }
          peer.handlerRef ! Message(modifiersSpec, Right(ModifiersData(modType, batch.toMap)), None)
          val remaining = mods.drop(batch.length)
          if (remaining.nonEmpty) {
            sendByParts(remaining)
          }
        }

        modifierSerializers.get(modType) match {
          case Some(serializer: BifrostSerializer[NodeViewModifier]) =>
            sendByParts(modifiers.map(m => m.id -> serializer.toBytes(m)))
          case _ =>
            log.error(s"Undefined serializer for modifier of type $modType")
        }
      }

    // check whether requested modifiers have been delivered to the local node from a remote peer
    case CheckDelivery(peerOpt, modifierTypeId, modifierId) =>
      // Do nothing, if modifier is already in a different state (it might be already received, applied, etc.),
      if (deliveryTracker.status(modifierId) == ModifiersStatus.Requested) {
        peerOpt match {
          // if a remote peer sent `Inv` for this modifier, wait for delivery from that peer until the number of checks exceeds the maximum
          case Some(peer) =>
            log.info(s"Peer ${peer.toString} has not delivered requested modifier ${encoder.encodeId(modifierId)} on time")
            penalizeNonDeliveringPeer(peer)
            deliveryTracker.onStillWaiting(peer, modifierTypeId, modifierId)
          case None =>
            // If original peer could not deliver the modifier we requested, ask another random peer
            // Since we need this modifier - no limit for number of attempts
            // todo: should we not have a limit on the number of asks? Could I be malicious and send 'Invs' for fake
            // todo: modifiers and then never respond?
            log.info(s"Modifier ${encoder.encodeId(modifierId)} was not delivered on time")
            deliveryTracker.setUnknown(modifierId)
            requestDownload(modifierTypeId, Seq(modifierId))
        }
      }
  }

  protected def viewHolderEvents: Receive = {
    case SuccessfulTransaction(tx) =>
      deliveryTracker.setHeld(tx.id)
      broadcastModifierInv(tx)

    case FailedTransaction(id, _, immediateFailure) =>
      val senderOpt = deliveryTracker.setInvalid(id)
      // penalize sender only in case transaction was invalidated at first validation.
      if (immediateFailure) senderOpt.foreach(penalizeMisbehavingPeer)

    case SyntacticallySuccessfulModifier(mod) =>
      deliveryTracker.setHeld(mod.id)

    case SyntacticallyFailedModification(mod, _) =>
      deliveryTracker.setInvalid(mod.id).foreach(penalizeMisbehavingPeer)

    case SemanticallySuccessfulModifier(mod) =>
      broadcastModifierInv(mod)

    case SemanticallyFailedModification(mod, _) =>
      deliveryTracker.setInvalid(mod.id).foreach(penalizeMisbehavingPeer)

    case ChangedHistory(reader: HR) =>
      historyReaderOpt = Some(reader)

    case ChangedMempool(reader: MR) =>
      mempoolReaderOpt = Some(reader)

    case ModifiersProcessingResult(applied: Seq[PMOD], cleared: Seq[PMOD]) =>
      // stop processing for cleared modifiers
      // applied modifiers state was already changed at `SyntacticallySuccessfulModifier`
      cleared.foreach(m => deliveryTracker.setUnknown(m.id))
      requestMoreModifiers(applied)
  }

  protected def peerManagerEvents: Receive = {
    case HandshakedPeer(remote) =>
      statusTracker.updateStatus(remote, Unknown)

    case DisconnectedPeer(remote) =>
      statusTracker.clearStatus(remote)
  }

  protected def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"NodeViewSynchronizer: got unexpected input $nonsense")
  }

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// METHOD DEFINITIONS ////////////////////////////////

  private def readersOpt: Option[(HR, MR)] = historyReaderOpt.flatMap(h => mempoolReaderOpt.map(mp => (h, mp)))

  protected def broadcastModifierInv[M <: NodeViewModifier](m: M): Unit = {
    val msg = Message(invSpec, Right(InvData(m.modifierTypeId, Seq(m.id))), None)
    networkControllerRef ! SendToNetwork(msg, Broadcast)
  }

  /**
    * Application-specific logic to request more modifiers after application if needed to
    * speed-up synchronization process, e.g. send Sync message for unknown or older peers
    * when our modifier is not synced yet, but no modifiers are expected from other peers
    * or request modifiers we need with known ids, that are not applied yet.
    */
  protected def requestMoreModifiers(applied: Seq[PMOD]): Unit = {}

  protected def sendSync(syncTracker: SyncTracker, history: HR): Unit = {
    val peers = statusTracker.peersToSyncWith()
    if (peers.nonEmpty) {
      networkControllerRef ! SendToNetwork(Message(syncInfoSpec, Right(history.syncInfo), None), SendToPeers(peers))
    }
  }

  // Send history extension to the (less developed) peer 'remote' which does not have it.
  def sendExtension(remote: ConnectedPeer,
                    status: HistoryComparisonResult,
                    ext: Seq[(ModifierTypeId, ModifierId)]): Unit =
    ext.groupBy(_._1).mapValues(_.map(_._2)).foreach {
      case (mid, mods) =>
        networkControllerRef ! SendToNetwork(Message(invSpec, Right(InvData(mid, mods)), None), SendToPeer(remote))
    }

  /**
    * Move `pmod` to `Invalid` if it is permanently invalid, to `Received` otherwise
    */
  @SuppressWarnings(Array("org.wartremover.warts.IsInstanceOf"))
  private def validateAndSetStatus(remote: ConnectedPeer, pmod: PMOD): Boolean = {
    historyReaderOpt match {
      case Some(hr) =>
        hr.applicableTry(pmod) match {
          case Failure(e) if e.isInstanceOf[MalformedModifierError] =>
            log.warn(s"Modifier ${pmod.id} is permanently invalid", e)
            deliveryTracker.setInvalid(pmod.id)
            penalizeMisbehavingPeer(remote)
            false
          case _ =>
            deliveryTracker.setReceived(pmod.id, remote)
            true
        }
      case None =>
        log.error("Got modifier while history reader is not ready")
        deliveryTracker.setReceived(pmod.id, remote)
        true
    }
  }

  /**
    * Parse modifiers using specified serializer, check that its id is equal to the declared one,
    * penalize misbehaving peer for every incorrect modifier,
    * call deliveryTracker.onReceive() for every correct modifier to update its status
    *
    * @return collection of parsed modifiers
    */
  private def parseModifiers[M <: NodeViewModifier](modifiers: Map[ModifierId, Array[Byte]],
                                                    serializer: BifrostSerializer[M],
                                                    remote: ConnectedPeer): Iterable[M] = {
    modifiers.flatMap { case (id, bytes) =>
      serializer.parseBytes(bytes) match {
        case Success(mod) if id == mod.id =>
          Some(mod)
        case _ =>
          // Penalize peer and do nothing - it will be switched to correct state on CheckDelivery
          penalizeMisbehavingPeer(remote)
          log.warn(s"Failed to parse modifier with declared id ${encoder.encodeId(id)} from ${remote.toString}")
          None
      }
    }
  }

  /**
    * Get modifiers from remote peer,
    * filter out spam modifiers and penalize peer for spam
    *
    * @return ids and bytes of modifiers that were requested by our node
    */
  private def processSpam(remote: ConnectedPeer,
                          typeId: ModifierTypeId,
                          modifiers: Map[ModifierId, Array[Byte]]): Map[ModifierId, Array[Byte]] = {

    val (requested, spam) = modifiers.partition { case (id, _) =>
      deliveryTracker.status(id) == Requested
    }

    if (spam.nonEmpty) {
      log.info(s"Spam attempt: peer $remote has sent a non-requested modifiers of type $typeId with ids" +
        s": ${spam.keys.map(encoder.encodeId)}")
      penalizeSpammingPeer(remote)
    }
    requested
  }

  protected def penalizeNonDeliveringPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.NonDeliveryPenalty)
  }

  protected def penalizeSpammingPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.SpamPenalty)
  }

  protected def penalizeMisbehavingPeer(peer: ConnectedPeer): Unit = {
    networkControllerRef ! PenalizePeer(peer.connectionId.remoteAddress, PenaltyType.MisbehaviorPenalty)
  }

  /**
    * Our node needs modifiers of type `modifierTypeId` with ids `modifierIds`
    * but peer that can deliver it is unknown.
    * Request this modifier from random peer.
    */
  protected def requestDownload(modifierTypeId: ModifierTypeId, modifierIds: Seq[ModifierId]): Unit = {
    deliveryTracker.setRequested(modifierIds, modifierTypeId, None)
    val msg = Message(requestModifierSpec, Right(InvData(modifierTypeId, modifierIds)), None)
    networkControllerRef ! SendToNetwork(msg, SendToRandom)
  }

}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object NodeViewSynchronizer {

  object Events {

    trait NodeViewSynchronizerEvent

    case object NoBetterNeighbour extends NodeViewSynchronizerEvent

    case object BetterNeighbourAppeared extends NodeViewSynchronizerEvent

  }

  object ReceivableMessages {

    // getLocalSyncInfo messages
    case object SendLocalSyncInfo

    case class ResponseFromLocal[M <: NodeViewModifier](source: ConnectedPeer, modifierTypeId: ModifierTypeId, localObjects: Seq[M])

    /**
      * Check delivery of modifier with type `modifierTypeId` and id `modifierId`.
      * `source` may be defined if we expect modifier from concrete peer or None if
      * we just need some modifier, but don't know who may it
      *
      */
    case class CheckDelivery(source: Option[ConnectedPeer],
                             modifierTypeId: ModifierTypeId,
                             modifierId: ModifierId)

    case class OtherNodeSyncingStatus[SI <: SyncInfo](remote: ConnectedPeer,
                                                      status: HistoryComparisonResult,
                                                      extension: Seq[(ModifierTypeId, ModifierId)])

    trait PeerManagerEvent

    case class HandshakedPeer(remote: ConnectedPeer) extends PeerManagerEvent

    case class DisconnectedPeer(remote: InetSocketAddress) extends PeerManagerEvent

    trait NodeViewHolderEvent

    trait NodeViewChange extends NodeViewHolderEvent

    case class ChangedHistory[HR <: HistoryReader[_ <: PersistentNodeViewModifier, _ <: SyncInfo]](reader: HR) extends NodeViewChange

    case class ChangedMempool[MR <: MemPoolReader[_ <: Transaction]](mempool: MR) extends NodeViewChange

    case class ChangedVault[VR <: VaultReader](reader: VR) extends NodeViewChange

    case class ChangedState[SR <: StateReader](reader: SR) extends NodeViewChange

    //todo: consider sending info on the rollback

    case object RollbackFailed extends NodeViewHolderEvent

    case class NewOpenSurface(newSurface: Seq[ModifierId]) extends NodeViewHolderEvent

    case class StartingPersistentModifierApplication[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends NodeViewHolderEvent

    /**
      * After application of batch of modifiers from cache to History, NodeViewHolder sends this message,
      * containing all just applied modifiers and cleared from cache
      */
    case class ModifiersProcessingResult[PMOD <: PersistentNodeViewModifier](applied: Seq[PMOD], cleared: Seq[PMOD])

    // hierarchy of events regarding modifiers application outcome
    trait ModificationOutcome extends NodeViewHolderEvent

    /**
      * @param immediateFailure - a flag indicating whether a transaction was invalid by the moment it was received.
      */
    case class FailedTransaction(transactionId: ModifierId, error: Throwable, immediateFailure: Boolean) extends ModificationOutcome

    case class SuccessfulTransaction[TX <: Transaction](transaction: TX) extends ModificationOutcome

    case class SyntacticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SemanticallyFailedModification[PMOD <: PersistentNodeViewModifier](modifier: PMOD, error: Throwable) extends ModificationOutcome

    case class SyntacticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

    case class SemanticallySuccessfulModifier[PMOD <: PersistentNodeViewModifier](modifier: PMOD) extends ModificationOutcome

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object NodeViewSynchronizerRef {
  def props[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MemPoolReader[TX] : ClassTag]
  (networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]])(implicit ec: ExecutionContext): Props =
    Props(new NodeViewSynchronizer[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef, syncInfoSpec,
      networkSettings, timeProvider, modifierSerializers))

  def apply[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MemPoolReader[TX] : ClassTag]
  (networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]])
  (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider, modifierSerializers))

  def apply[TX <: Transaction,
  SI <: SyncInfo,
  SIS <: SyncInfoMessageSpec[SI],
  PMOD <: PersistentNodeViewModifier,
  HR <: HistoryReader[PMOD, SI] : ClassTag,
  MR <: MemPoolReader[TX] : ClassTag]
  (name: String,
   networkControllerRef: ActorRef,
   viewHolderRef: ActorRef,
   syncInfoSpec: SIS,
   networkSettings: NetworkSettings,
   timeProvider: NetworkTimeProvider,
   modifierSerializers: Map[ModifierTypeId, BifrostSerializer[_ <: NodeViewModifier]])
  (implicit system: ActorSystem, ec: ExecutionContext): ActorRef =
    system.actorOf(props[TX, SI, SIS, PMOD, HR, MR](networkControllerRef, viewHolderRef,
      syncInfoSpec, networkSettings, timeProvider, modifierSerializers), name)
}
