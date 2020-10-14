package co.topl.consensus

import akka.actor._
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ Coinbase, Transaction }
import co.topl.nodeView.CurrentView
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{ GetDataFromCurrentView, LocallyGeneratedModifier }
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.settings.{ AppContext, AppSettings, NodeViewReady }
import co.topl.utils.Logging
import co.topl.utils.TimeProvider.Time

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.FiniteDuration
import scala.util.{ Failure, Success, Try }

/**
 * Forger takes care of attempting to create new blocks using the wallet provided in the NodeView
 * Must be singleton
 */
class Forger (settings: AppSettings, appContext: AppContext )
             ( implicit ec: ExecutionContext ) extends Actor with Logging {

  //type HR = HistoryReader[Block, BifrostSyncInfo]

  // Import the types of messages this actor RECEIVES
  import Forger.ReceivableMessages._

  // version byte used for creating blocks
  private val blockVersion = settings.application.version.firstDigit

  // holder of private keys that are used to forge
  private val keyFileDir = settings.application.keyFileDir.ensuring(_.isDefined, "A keyfile directory must be specified").get
  private val keyRing = KeyRing(keyFileDir)

  // flag to enable forging on a private network
  private lazy val isPrivateForging = appContext.networkType.isPrivateForger

  // a timestamp updated on each forging attempt
  private var forgeTime: Time = appContext.timeProvider.time()

  // a map of ledger providers that expose functionality to handle different requests from consensus
  private val ledgerProviders: mutable.Map[String, ActorRef] = mutable.Map[String, ActorRef]()

  // setting to limit the size of blocks
  val TransactionsInBlock = 100 //todo: JAA - should be a part of consensus, but for our app is okay

  override def preStart ( ): Unit = {
    //register for application initialization message
    context.system.eventStream.subscribe(self, classOf[NodeViewReady])
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    initialization orElse
      nonsense

  private def readyToForge: Receive = {
    readyHandlers orElse
      keyManagement orElse
      nonsense
  }

  private def activeForging: Receive =
    activeHandlers orElse
      keyManagement orElse
      nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  private def initialization: Receive = {
    case RegisterLedgerProvider => ledgerProviders += "nodeViewHolder" -> sender
    case CreateGenesisKeys(num) => sender() ! generateGenesisKeys(num)
    case params: GenesisParams  => setGenesisParameters(params)
    case NodeViewReady() =>
      log.info(s"${Console.YELLOW}Forger transitioning to the operational state${Console.RESET}")
      context become readyToForge
      checkPrivateForging()
  }

  private def readyHandlers: Receive = {
    case StartForging =>
      log.info("Received a START signal, forging will commence shortly.")
      scheduleForgingAttempt() // schedule the next forging attempt
      context become activeForging

    case StopForging =>
      log.warn(s"Received a STOP signal while not forging. Signal ignored")
  }

  private def activeHandlers: Receive = {
    case StartForging =>
      log.warn(s"Forger: Received a START signal while forging. Signal ignored")

    case StopForging =>
      log.info(s"Forger: Received a stop signal. Forging will terminate after this trial")
      context become readyToForge

    case CurrentView(h: History, s: State, m: MemPool) =>
      ledgerProviders.get("nodeViewHolder") match {
        case Some(provider) =>
          updateForgeTime() // update the forge timestamp
          tryForging(h, s, m, provider) // initiate forging attempt
          scheduleForgingAttempt() // schedule the next forging attempt

        case None =>
          log.warn(s"Ledger provider not available, skipping forging attempt.")
      }
  }

  private def keyManagement: Receive = {
    case UnlockKey(addr, password)           => sender() ! keyRing.unlockKeyFile(addr, password)
    case LockKey(addr, password)             => sender() ! keyRing.lockKeyFile(addr, password)
    case CreateKey(password)                 => sender() ! keyRing.generateKeyFile(password)
    case ImportKey(password, mnemonic, lang) => sender() ! keyRing.importPhrase(password, mnemonic, lang)
    case ListKeys                            => sender() ! keyRing.publicKeys
  }

  private def nonsense: Receive = {
    case nonsense: Any =>
      log.warn(s"Got unexpected input $nonsense from ${sender()}")
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  /** Schedule a forging attempt */
  private def scheduleForgingAttempt ( ): Unit = {
    ledgerProviders.get("nodeViewHolder") match {
      case Some(ref) => context.system.scheduler.scheduleOnce(settings.forging.blockGenerationDelay)(ref ! GetDataFromCurrentView)
      case None      => log.warn(s"Ledger provider not available, skipping schedule.")
    }
  }

  /** Updates the forging actors timestamp */
  private def updateForgeTime ( ): Unit = forgeTime = appContext.timeProvider.time()

  /** Helper function to enable private forging if we can expects keys in the key ring */
  private def checkPrivateForging (): Unit = if (isPrivateForging && keyRing.publicKeys.nonEmpty) self ! StartForging

  /** Helper function to generate a set of keys used for the genesis block (for private test networks) */
  private def generateGenesisKeys (num: Int): Set[PublicKey25519Proposition] =
    keyRing.generateNewKeyPairs(num) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    }

  /** Sets the genesis network parameters for calculating adjusted difficulty */
  private def setGenesisParameters (parameters: GenesisParams): Unit = {
    targetBlockTime = parameters.targetBlockTime
    maxStake = parameters.totalStake
    initialDifficulty = parameters.initialDifficulty
  }

  /**
   * Primary method for attempting to forge a new block and publish it to the network
   *
   * @param history history instance for gathering chain parameters
   * @param state   state instance for semantic validity tests of transactions
   * @param memPool mempool instance for picking transactions to include in the block if created
   */
  private def tryForging ( history: History, state: State, memPool: MemPool, ledgerRef: ActorRef): Unit = {
    log.info(s"${Console.CYAN}Trying to generate a new block, chain length: ${history.height}${Console.RESET}")
    log.info("chain difficulty: " + history.difficulty)

    try {
      // get the set of boxes to use for testing
      val boxes = getArbitBoxes(state) match {
        case Success(boxes) => boxes
        case Failure(ex)    => throw ex
      }

      log.debug(s"Trying to generate block on top of ${history.bestBlock.id} with balance " +
        s"${boxes.map(_.value).sum}")

      // create the coinbase reward transaction
      val coinbase = createCoinbase(history.bestBlock.id) match {
        case Success(cb) => cb
        case Failure(ex) => throw ex
      }

      // pick the transactions from the mempool for inclusion in the block (if successful)
      val transactions = pickTransactions(memPool, state) match {
        case Success(txs) => txs
        case Failure(ex)  => throw ex
      }

      // check forging eligibility
      leaderElection(history.bestBlock, history.difficulty, boxes, coinbase, transactions, blockVersion) match {
        case Some(block) =>
          log.debug(s"Locally generated block: $block")
          ledgerRef ! LocallyGeneratedModifier[Block](block)

        case None => log.debug(s"Failed to generate block")
      }
    } catch {
      case ex: Throwable =>
        log.warn(s"Disabling forging due to exception: $ex. Resolve forging error and try forging again.")
        self ! StopForging
      }
  }

  /**
   * Get the set of Arbit boxes owned by all unlocked keys in the key ring
   *
   * @param state state instance used to lookup the balance for all unlocked keys
   * @return a set of arbit boxes to use for testing leadership eligibility
   */
  private def getArbitBoxes ( state: State ): Try[Set[ArbitBox]] = Try {
    val publicKeys = keyRing.publicKeys

    if ( publicKeys.nonEmpty ) {
      publicKeys.flatMap {
        state.getTokenBoxes(_)
          .getOrElse(Seq())
          .collect { case box: ArbitBox => box }
      }
    } else {
      throw new Error("No boxes available for forging!")
    }
  }


  /**
   * Attempt to create an unsigned coinbase transaction that will distribute the block reward
   * if forging is successful
   *
   * @param parentId block id of the current head of the chain
   * @return an unsigned coinbase transaction
   */
  private def createCoinbase ( parentId: Block.BlockId ): Try[Coinbase] = Try {
    //todo: JAA - we may want to reconsider how to specify the reward address
    val rewardAddr = keyRing.publicKeys.headOption match {
      case Some(pk) => pk
      case _        => throw new Error("Attempted to forge but no keyfiles are unlocked!")
    }

    Coinbase.createRaw(rewardAddr, inflation, forgeTime, parentId)
  }

  /**
   * Pick a set of transactions from the mempool that result in a valid state when applied to the current state
   *
   * @param memPool the set of pending transactions
   * @param state   state to use for semantic validity checking
   * @return a sequence of valid transactions
   */
  private def pickTransactions ( memPool: MemPool,
                                 state  : State
                               ): Try[Seq[Transaction]] = Try {

    memPool.take(TransactionsInBlock).foldLeft(Seq[Transaction]()) { case (txAcc, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txAcc.flatMap(_.boxIdsToOpen).contains(id))
      val validBoxes = tx.newBoxes.forall(b ⇒ state.getBox(b.id).isEmpty)

      if ( validBoxes ) memPool.remove(tx)

      state.validate(tx) match {
        case Success(_) if txNotIncluded => txAcc :+ tx
        case Success(_)                  => txAcc
        case Failure(ex)                 =>
          log.debug(s"${Console.RED}Invalid Unconfirmed transaction $tx. Removing transaction${Console.RESET}. Failure: $ex")
          txAcc
      }
    }
  }

  /**
   * Performs the leader election procedure and returns a block if successful
   *
   * @param parent       block to forge on top of
   * @param difficulty   base difficulty of the parent block
   * @param boxes        set of Arbit boxes to attempt to forge with
   * @param txsToInclude sequence of transactions for inclusion in the block body
   * @param version      version tag for inclusion in the block
   * @return a block if the leader election is successful (none if test failed)
   */
  private def leaderElection ( parent: Block,
                               difficulty: Long,
                               boxes     : Set[ArbitBox],
                               coinbase  : Coinbase,
                               txsToInclude: Seq[Transaction],
                               version     : Block.Version
                             ): Option[Block] = {

    val target = calcAdjustedTarget(parent, difficulty, forgeTime)

    // test procedure to determine eligibility
    val successfulHits = boxes.map { box =>
      (box, calcHit(parent)(box))
    }.filter { test =>
      BigInt(test._2) < (test._1.value * target).toBigInt
    }

    log.debug(s"Successful hits: ${successfulHits.size}")

    successfulHits.headOption.flatMap { case (box, _) =>
      keyRing.secretByPublicImage(box.proposition) match {
        case Some(sk) =>
          // use the secret key that owns the successful box to sign the coinbase transaction
          val signedCb = coinbase.copy(signatures = Map(sk.publicImage -> sk.sign(coinbase.messageToSign)))

          // add the signed coinbase transaction to the block and return
          Some(Block.create(parent.id, forgeTime, signedCb +: txsToInclude, box, sk, version))

        case _ =>
          log.warn(s"Could not find the secret for public image ${box.proposition}. Failed to forge block")
          None
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object Forger {

  object ReceivableMessages {

    case object RegisterLedgerProvider

    case class CreateGenesisKeys (num: Int)

    case class GenesisParams (totalStake: Long, targetBlockTime: FiniteDuration, initialDifficulty: Long)

    case object StartForging

    case object StopForging

    case class UnlockKey (addr: String, password: String)

    case class LockKey (addr: String, password: String)

    case object ListKeys

    case class CreateKey (password: String)

    case class ImportKey (password: String, mnemonic: String, lang: String)

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object ForgerRef {
  def props ( settings: AppSettings, appContext: AppContext )
            ( implicit ec: ExecutionContext ): Props =
    Props(new Forger(settings, appContext))

  def apply ( settings: AppSettings, appContext: AppContext )
            ( implicit system: ActorSystem, ec: ExecutionContext ): ActorRef =
    system.actorOf(props(settings, appContext))

  def apply ( name: String, settings: AppSettings, appContext: AppContext )
            ( implicit system: ActorSystem, ec: ExecutionContext ): ActorRef =
    system.actorOf(props(settings, appContext), name)
}