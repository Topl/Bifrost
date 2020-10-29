package co.topl.consensus

import akka.actor._
import akka.util.Timeout
import co.topl.consensus.Forger.ChainParams
import co.topl.consensus.genesis.{PrivateTestnet, Toplnet}
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{Coinbase, Transaction}
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{GetDataFromCurrentView, LocallyGeneratedModifier}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.nodeView.state.box.ArbitBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.{CurrentView, NodeViewHolder}
import co.topl.settings.NetworkType.{DevNet, LocalNet, MainNet, PrivateNet, TestNet}
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.Logging
import co.topl.utils.TimeProvider.Time

import scala.concurrent.ExecutionContext
import scala.util.{Failure, Success, Try}

/**
 * Forger takes care of attempting to create new blocks using the wallet provided in the NodeView
 * Must be singleton
 */
class Forger (settings: AppSettings, appContext: AppContext )
             ( implicit ec: ExecutionContext ) extends Actor with Logging {

  //type HR = HistoryReader[Block, BifrostSyncInfo]

  // Import the types of messages this actor RECEIVES
  import Forger.ReceivableMessages._

  // holder of private keys that are used to forge
  private val keyFileDir = settings.application.keyFileDir.ensuring(_.isDefined, "A keyfile directory must be specified").get
  private val keyRing = KeyRing(keyFileDir)

  // a timestamp updated on each forging attempt
  private var forgeTime: Time = appContext.timeProvider.time()

  override def preStart (): Unit = {
    // determine the set of applicable protocol rules for this software version
    protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)

    //register for application initialization message
    context.system.eventStream.subscribe(self, NodeViewReady.getClass)
    context.system.eventStream.subscribe(self, GenerateGenesis.getClass)
  }

  ////////////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// ACTOR MESSAGE HANDLING //////////////////////////////

  // ----------- CONTEXT
  override def receive: Receive =
    initialization orElse
      nonsense

  private def readyToForge: Receive =
    readyHandlers orElse
      keyManagement orElse
      nonsense

  private def activeForging: Receive =
    activeHandlers orElse
      keyManagement orElse
      nonsense

  // ----------- MESSAGE PROCESSING FUNCTIONS
  private def initialization: Receive = {
    case GenerateGenesis => sender() ! initializeGenesis
    case NodeViewReady   =>
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

    case CurrentView(history: History, state: State, mempool: MemPool) =>
      updateForgeTime() // update the forge timestamp
      tryForging(history, state, mempool) // initiate forging attempt
      scheduleForgingAttempt() // schedule the next forging attempt
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
  /** Updates the forging actors timestamp */
  private def updateForgeTime (): Unit = forgeTime = appContext.timeProvider.time()

  /** Helper function to enable private forging if we can expects keys in the key ring */
  private def checkPrivateForging (): Unit =
    if (appContext.networkType.startWithForging && keyRing.publicKeys.nonEmpty) self ! StartForging

  /** Schedule a forging attempt */
  private def scheduleForgingAttempt (): Unit = {
    implicit val timeout: Timeout = Timeout(settings.forging.blockGenerationDelay)
    // going to go with actorSelection for now but the block creation needs to be moved to the ledger layer
    context.actorSelection("../" + NodeViewHolder.actorName).resolveOne().onComplete {
      case Success(nvh: ActorRef) =>
        context.system.scheduler.scheduleOnce(settings.forging.blockGenerationDelay)(nvh ! GetDataFromCurrentView)
      case _                      =>
        log.warn("No ledger actor found. Stopping forging attempts")
        self ! StopForging
    }
  }

  /** Helper function to generate a set of keys used for the genesis block (for private test networks) */
  private def generateKeys (num: Int, seed: Option[String] = None): Set[PublicKey25519Proposition] = {
    keyRing.generateNewKeyPairs(num, seed) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    }
  }

  /** Return the correct genesis parameters for the chosen network.
    * NOTE: the default private network is set in AppContext so the fall-through should result in an error.
    */
  private def initializeGenesis: Try[Block] = {
    ( appContext.networkType match {
      case MainNet(opts)    => Toplnet.getGenesisBlock
      case TestNet(opts)    => ???
      case DevNet(opts)     => ???
      case LocalNet(opts)   => PrivateTestnet(generateKeys, settings, opts).getGenesisBlock
      case PrivateNet(opts) => PrivateTestnet(generateKeys, settings, opts).getGenesisBlock
      case _                => throw new Error("Undefined network type.")
    } ).map {
      case (block: Block, ChainParams(totalStake, initDifficulty)) =>
        maxStake = totalStake
        difficulty = initDifficulty
        height = 0

        block
    }
  }

  /**
   * Primary method for attempting to forge a new block and publish it to the network
   *
   * @param history history instance for gathering chain parameters
   * @param state   state instance for semantic validity tests of transactions
   * @param memPool mempool instance for picking transactions to include in the block if created
   */
  private def tryForging ( history: History, state: State, memPool: MemPool): Unit = {
    log.debug(s"${Console.YELLOW}Attempting to forge with settings ${protocolMngr.current(history.height)}${Console.RESET}")
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

      require(boxes.map(_.value).sum > 0, "No Arbits could be found to stake with, exiting attempt")

      // create the coinbase reward transaction
      val coinbase = createCoinbase(history.bestBlock.id) match {
        case Success(cb) => cb
        case Failure(ex) => throw ex
      }

      // pick the transactions from the mempool for inclusion in the block (if successful)
      val transactions = pickTransactions(memPool, state, history.height) match {
        case Success(txs) => txs
        case Failure(ex)  => throw ex
      }
      // check forging eligibility
      leaderElection(history.bestBlock, history.height, history.difficulty, boxes, coinbase, transactions) match {
        case Some(block) =>
          log.debug(s"Locally generated block: $block ${block.json}")
          context.system.eventStream.publish(LocallyGeneratedModifier[Block](block))

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
                                 state  : State,
                                 chainHeight: Long
                               ): Try[Seq[Transaction]] = Try {

    memPool.take(numTxInBlock(chainHeight)).foldLeft(Seq[Transaction]()) { case (txAcc, tx) =>
      val txNotIncluded = tx.boxIdsToOpen.forall(id => !txAcc.flatMap(_.boxIdsToOpen).contains(id))
      val validBoxes = tx.newBoxes.forall(b ⇒ state.getBox(b.id).isEmpty)

      if ( !validBoxes ) memPool.remove(tx)

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
   * @param parentHeight height of the block being forged on top of
   * @param parentDifficulty   base difficulty of the parent block
   * @param boxes        set of Arbit boxes to attempt to forge with
   * @param txsToInclude sequence of transactions for inclusion in the block body
   * @return a block if the leader election is successful (none if test failed)
   */
  private def leaderElection ( parent: Block,
                               parentHeight: Long,
                               parentDifficulty: Long,
                               boxes     : Set[ArbitBox],
                               coinbase  : Coinbase,
                               txsToInclude: Seq[Transaction]
                             ): Option[Block] = {

    val target = calcAdjustedTarget(parent, parentHeight, parentDifficulty, forgeTime)
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
          Some(Block.create(parent.id, forgeTime, signedCb +: txsToInclude, box, sk, blockVersion(parentHeight + 1)))

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

  val actorName = "forger"

  case class ChainParams ( totalStake: Long, difficulty: Long)

  object ReceivableMessages {

    case object GenerateGenesis

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