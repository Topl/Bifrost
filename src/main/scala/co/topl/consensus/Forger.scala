package co.topl.consensus

import akka.Done
import akka.actor._
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.akka.AskException
import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.attestation.{Address, AddressEncoder, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.Forger.{ChainParams, PickTransactionsResult}
import co.topl.consensus.genesis.{HelGenesis, PrivateGenesis, ToplnetGenesis, ValhallaGenesis}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.CurrentView
import co.topl.nodeView.NodeViewHolder.ReceivableMessages.{
  EliminateTransactions,
  GetDataFromCurrentView,
  LocallyGeneratedModifier
}
import co.topl.nodeView.history.History
import co.topl.nodeView.mempool.MemPool
import co.topl.nodeView.state.State
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.NetworkType._
import co.topl.utils.TimeProvider.Time
import co.topl.utils.{Int128, Logging, TimeProvider}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/** Forger takes care of attempting to create new blocks using the wallet provided in the NodeView
  * Must be singleton
  */
class Forger(settings: AppSettings, appContext: AppContext)(implicit ec: ExecutionContext, np: NetworkPrefix)
    extends Actor
    with Logging {

  //type HR = HistoryReader[Block, BifrostSyncInfo]
  type TX = Transaction.TX

  // Import the types of messages this actor RECEIVES
  import Forger.ReceivableMessages._

  // holder of private keys that are used to forge
  private val keyFileDir =
    settings.application.keyFileDir.ensuring(_.isDefined, "A keyfile directory must be specified").get
  private val keyRing = KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)

  // the nodeViewHolder actor ref for retrieving the current state
  private var nodeViewHolderRef: Option[ActorRef] = None

  // designate a rewards address
  private var rewardAddress: Option[Address] =
    settings.forging.rewardsAddress.flatMap {
      AddressEncoder.fromStringWithCheck(_, appContext.networkType.netPrefix) match {
        case Left(ex) =>
          log.warn(s"${Console.YELLOW}Unable to set rewards address due to $ex ${Console.RESET}")
          None

        case Right(addr) => Some(addr)
      }
    }

  // a timestamp updated on each forging attempt
  private var forgeTime: Time = appContext.timeProvider.time

  override def preStart(): Unit = {
    // determine the set of applicable protocol rules for this software version
    protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)

    //register for application initialization message
    context.system.eventStream.subscribe(self, classOf[NodeViewReady])
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
    case NodeViewReady(nvhRef: ActorRef) =>
      log.info(s"${Console.YELLOW}Forger transitioning to the operational state${Console.RESET}")
      nodeViewHolderRef = Some(nvhRef)
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
    case CreateKey(password)                 => sender() ! keyRing.DiskOps.generateKeyFile(password)
    case UnlockKey(addr, password)           => sender() ! keyRing.DiskOps.unlockKeyFile(addr, password)
    case LockKey(addr)                       => sender() ! keyRing.removeFromKeyring(addr)
    case ImportKey(password, mnemonic, lang) => sender() ! keyRing.importPhrase(password, mnemonic, lang)
    case ListKeys                            => sender() ! keyRing.addresses
    case UpdateRewardsAddress(address)       => sender() ! updateRewardsAddress(address)
    case GetRewardsAddress                   => sender() ! rewardAddress.fold("none")(_.toString)
  }

  private def nonsense: Receive = { case nonsense: Any =>
    log.warn(s"Got unexpected input $nonsense from ${sender()}")
  }

  ////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// METHOD DEFINITIONS ////////////////////////////////
  /** Updates the forging actors timestamp */
  private def updateForgeTime(): Unit = forgeTime = appContext.timeProvider.time

  /** Updates the rewards address from the API */
  private def updateRewardsAddress(address: Address): String = {
    rewardAddress = Some(address)
    rewardAddress.fold("none")(_.toString)
  }

  /** Helper function to enable private forging if we can expects keys in the key ring */
  private def checkPrivateForging(): Unit = {
    val sf = settings.forging
    if (sf.forgeOnStartup) {
      // if forging on startup is enabled, check if a seed was provided and the keyring is not already populated
      // this is usually the case when you have started up a private network and are attempting to resume it using
      // the same seed you used previously to continue forging
      if (sf.privateTestnet.flatMap(_.genesisSeed).nonEmpty && keyRing.addresses.isEmpty) {
        val sfp = sf.privateTestnet.get //above conditional ensures this exists
        generateKeys(sfp.numTestnetAccts, sfp.genesisSeed) // JAA - hacky way to reproduce keys (not fully tested)
        if (rewardAddress.isEmpty) rewardAddress = keyRing.addresses.headOption
        maxStake = sfp.numTestnetAccts * sfp.testnetBalance // JAA - we need to save these values to disk
      }

      // if forging has been enabled and the keyring is nonEmpty (either from the call above or genesis block formation)
      // then we should send the StartForging signal
      if (keyRing.addresses.nonEmpty) self ! StartForging
      else log.warn("Forging process not started since the key ring is empty")
    }
  }

  /** Schedule a forging attempt */
  private def scheduleForgingAttempt(): Unit =
    nodeViewHolderRef match {
      case Some(nvh: ActorRef) =>
        context.system.scheduler.scheduleOnce(settings.forging.blockGenerationDelay)(nvh ! GetDataFromCurrentView)

      case _ =>
        log.warn("No ledger actor found. Stopping forging attempts")
        self ! StopForging
    }

  /** Helper function to generate a set of keys used for the genesis block (for private test networks) */
  private def generateKeys(num: Int, seed: Option[String] = None): Set[PublicKeyPropositionCurve25519] =
    keyRing.generateNewKeyPairs(num, seed) match {
      case Success(keys) => keys.map(_.publicImage)
      case Failure(ex)   => throw ex
    }

  /** Return the correct genesis parameters for the chosen network.
    * NOTE: the default private network is set in AppContext so the fall-through should result in an error.
    */
  private def initializeGenesis: Try[Block] =
    (appContext.networkType match {
      case Mainnet         => ToplnetGenesis.getGenesisBlock
      case ValhallaTestnet => ValhallaGenesis.getGenesisBlock
      case HelTestnet      => HelGenesis.getGenesisBlock
      case LocalTestnet    => PrivateGenesis(generateKeys, settings).getGenesisBlock
      case PrivateTestnet  => PrivateGenesis(generateKeys, settings).getGenesisBlock
      case _               => throw new Error("Undefined network type.")
    }).map { case (block: Block, ChainParams(totalStake, initDifficulty)) =>
      if (rewardAddress.isEmpty) rewardAddress = keyRing.addresses.headOption
      maxStake = totalStake
      difficulty = initDifficulty
      height = 0

      block
    }

  /** Primary method for attempting to forge a new block and publish it to the network
    *
    * @param history history instance for gathering chain parameters
    * @param state   state instance for semantic validity tests of transactions
    * @param memPool mempool instance for picking transactions to include in the block if created
    */
  private def tryForging(history: History, state: State, memPool: MemPool): Unit = {
    log.debug(
      s"${Console.MAGENTA}Attempting to forge with settings ${protocolMngr.current(history.height)} " +
      s"and from addresses: ${keyRing.addresses}${Console.RESET}"
    )
    log.info(
      s"${Console.CYAN}Trying to generate a new block on top of ${history.bestBlock.id}. Parent has " +
      s"height ${history.height} and difficulty ${history.difficulty} ${Console.RESET}"
    )

    try {
      val rewardAddr = rewardAddress.getOrElse(throw new Error("No rewards address specified"))

      // get the set of boxes to use for testing
      val boxes = getArbitBoxes(state) match {
        case Success(bx) => bx
        case Failure(ex) => throw ex
      }

      log.debug(s"Trying to generate block from total stake ${boxes.map(_.value.quantity).sum}")
      require(
        boxes.map(_.value.quantity).sum > 0,
        "No Arbits could be found to stake with, exiting attempt"
      )

      // create the coinbase reward transaction
      val arbitReward = createArbitReward(rewardAddr, history.bestBlock.id) match {
        case Success(cb) => cb
        case Failure(ex) => throw ex
      }

      // pick the transactions from the mempool for inclusion in the block (if successful)
      val transactions = pickTransactions(memPool, state, history.height) match {
        case Success(res) =>
          if (res.toEliminate.nonEmpty) nodeViewHolderRef.foreach(_ ! EliminateTransactions(res.toEliminate.map(_.id)))
          res.toApply

        case Failure(ex) => throw ex
      }

      // create the unsigned fee reward transaction
      val polyReward =
        createPolyReward(transactions.map(_.fee).sum, rewardAddr, history.bestBlock.id) match {
          case Success(tx) => tx
          case Failure(ex) => throw ex
        }

      // retrieve the latest TWO block times for updating the difficulty if we forge a new blow
      val prevTimes = history.getTimestampsFrom(history.bestBlock, nxtBlockNum)

      // check forging eligibility
      leaderElection(history.bestBlock, prevTimes, boxes, Seq(arbitReward, polyReward), transactions) match {
        case Some(block) =>
          log.debug(s"Locally generated block: $block")
          context.system.eventStream.publish(LocallyGeneratedModifier[Block](block))

        case _ => log.debug(s"Failed to generate block")
      }
    } catch {
      case ex: Throwable =>
        log.warn(s"Disabling forging due to exception: $ex. Resolve forging error and try forging again.")
        self ! StopForging
    }
  }

  /** Get the set of Arbit boxes owned by all unlocked keys in the key ring
    *
    * @param state state instance used to lookup the balance for all unlocked keys
    * @return a set of arbit boxes to use for testing leadership eligibility
    */
  private def getArbitBoxes(state: State): Try[Seq[ArbitBox]] = Try {
    if (keyRing.addresses.nonEmpty) {
      keyRing.addresses.flatMap {
        state
          .getTokenBoxes(_)
          .getOrElse(Seq())
          .collect { case box: ArbitBox => box }
      }.toSeq
    } else {
      throw new Error("No boxes available for forging!")
    }
  }

  /** Attempt to create an unsigned coinbase transaction that will distribute the block reward
    * if forging is successful
    *
    * @return an unsigned coinbase transaction
    */
  private def createArbitReward(
    rewardAdr: Address,
    parentId:  ModifierId
  ): Try[ArbitTransfer[PublicKeyPropositionCurve25519]] =
    Try {
      ArbitTransfer(
        IndexedSeq(),
        IndexedSeq((rewardAdr, SimpleValue(inflation))),
        Map[PublicKeyPropositionCurve25519, SignatureCurve25519](),
        Int128(0),
        forgeTime,
        Some(parentId.toString + "_"), // the underscore is for letting miners add their own message in the future
        minting = true
      )
    }

  private def createPolyReward(
    amount:    Int128,
    rewardAdr: Address,
    parentId:  ModifierId
  ): Try[PolyTransfer[PublicKeyPropositionCurve25519]] =
    Try {
      PolyTransfer(
        IndexedSeq(),
        IndexedSeq((rewardAdr, SimpleValue(amount))),
        Map[PublicKeyPropositionCurve25519, SignatureCurve25519](),
        0,
        forgeTime,
        Some(parentId.toString + "_"), // the underscore is for letting miners add their own message in the future
        minting = true
      )
    }

  /** Pick a set of transactions from the mempool that result in a valid state when applied to the current state
    *
    * @param memPool the set of pending transactions
    * @param state   state to use for semantic validity checking
    * @return a sequence of valid transactions
    */
  private def pickTransactions(memPool: MemPool, state: State, chainHeight: Long): Try[PickTransactionsResult] = Try {

    memPool
      .take[Int128](numTxInBlock(chainHeight))(-_.tx.fee) // returns a sequence of transactions ordered by their fee
      .filter(
        _.tx.fee > settings.forging.minTransactionFee
      ) // default strategy ignores zero fee transactions in mempool
      .foldLeft(PickTransactionsResult(Seq(), Seq())) { case (txAcc, utx) =>
        // ensure that each transaction opens a unique box by checking that this transaction
        // doesn't open a box already being opened by a previously included transaction
        val boxAlreadyUsed = utx.tx.boxIdsToOpen.exists(id => txAcc.toApply.flatMap(_.boxIdsToOpen).contains(id))

        // if any newly created box matches a box already in the UTXO set in state, remove the transaction
        val boxAlreadyExists = utx.tx.newBoxes.exists(b => state.getBox(b.id).isDefined)

        (boxAlreadyUsed, boxAlreadyExists) match {
          case (false, false) =>
            state.semanticValidate(utx.tx) match {
              case Success(_) => PickTransactionsResult(txAcc.toApply :+ utx.tx, txAcc.toEliminate)
              case Failure(ex) =>
                log.debug(
                  s"${Console.RED}Transaction ${utx.tx.id} failed semantic validation. " +
                  s"Transaction will be removed.${Console.RESET} Failure: $ex"
                )
                PickTransactionsResult(txAcc.toApply, txAcc.toEliminate :+ utx.tx)
            }

          case (_, true) =>
            log.debug(
              s"${Console.RED}Transaction ${utx.tx.id} was rejected from the forger transaction queue" +
              s" because a newly created box already exists in state. The transaction will be removed."
            )
            PickTransactionsResult(txAcc.toApply, txAcc.toEliminate :+ utx.tx)

          case (true, _) =>
            log.debug(
              s"${Console.RED}Transaction ${utx.tx.id} was rejected from forger transaction queue" +
              s" because a box was used already in a previous transaction. The transaction will be removed."
            )
            PickTransactionsResult(txAcc.toApply, txAcc.toEliminate :+ utx.tx)
        }
      }
  }

  /** Performs the leader election procedure and returns a block if successful
    *
    * @param parent       block to forge on top of
    * @param boxes        set of Arbit boxes to attempt to forge with
    * @param txsToInclude sequence of transactions for inclusion in the block body
    * @return a block if the leader election is successful (none if test failed)
    */
  private def leaderElection(
    parent:       Block,
    prevTimes:    Vector[TimeProvider.Time],
    boxes:        Seq[ArbitBox],
    rawRewards:   Seq[TX],
    txsToInclude: Seq[TX]
  ): Option[Block] = {

    val target = calcAdjustedTarget(parent, parent.height, parent.difficulty, forgeTime)

    // test procedure to determine eligibility
    val successfulHits = boxes
      .map { box =>
        (box, calcHit(parent)(box))
      }
      .filter { test =>
        BigInt(test._2) < (test._1.value.quantity.doubleValue() * target).toBigInt
      }

    log.debug(s"Successful hits: ${successfulHits.size}")

    successfulHits.headOption.flatMap { case (box, _) =>
      {
        // generate the address the owns the generator box
        val matchingAddr = Address(box.evidence)

        // lookup the public associated with the box,
        // (this is separate from the signing function so that the private key never leaves the KeyRing)
        val publicKey: PublicKeyPropositionCurve25519 = keyRing.lookupPublicKey(matchingAddr) match {
          case Success(pk) => pk
          case Failure(ex) => throw ex
        }

        // use the private key that owns the generator box to create a function that will sign the new block
        val signingFunction: Array[Byte] => Try[SignatureCurve25519] =
          (messageToSign: Array[Byte]) => keyRing.signWithAddress(matchingAddr)(messageToSign)

        // use the secret key that owns the successful box to sign the rewards transactions
        val getAttMap: TX => Map[PublicKeyPropositionCurve25519, SignatureCurve25519] = (tx: TX) => {
          val sig = signingFunction(tx.messageToSign) match {
            case Success(sig) => sig
            case Failure(ex)  => throw ex
          }
          Map(publicKey -> sig)
        }

        val signedRewards = rawRewards.map {
          case tx: ArbitTransfer[_] => tx.copy(attestation = getAttMap(tx))
          case tx: PolyTransfer[_]  => tx.copy(attestation = getAttMap(tx))
        }

        // calculate the newly forged blocks updated difficulty
        val newDifficulty = calcNewBaseDifficulty(parent.height + 1, parent.difficulty, prevTimes :+ forgeTime)

        // add the signed coinbase transaction to the block, sign it, and return the newly forged block
        Block.createAndSign(
          parent.id,
          forgeTime,
          signedRewards ++ txsToInclude,
          box,
          publicKey,
          parent.height + 1,
          newDifficulty,
          blockVersion(parent.height + 1)
        )(signingFunction)

      } match {
        case Success(block) => Some(block)
        case Failure(ex) =>
          log.warn(s"A successful hit was found but failed to forge block due to exception: $ex")
          None
      }
    }
  }
}

////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// COMPANION SINGLETON ////////////////////////////////

object Forger {

  val actorName = "forger"

  case class ChainParams(totalStake: Int128, difficulty: Long)

  case class PickTransactionsResult(toApply: Seq[Transaction.TX], toEliminate: Seq[Transaction.TX])

  object ReceivableMessages {

    case object GenerateGenesis

    case object StartForging

    case object StopForging

    case class UnlockKey(addr: String, password: String)

    case class LockKey(addr: Address)

    case object ListKeys

    case class CreateKey(password: String)

    case class ImportKey(password: String, mnemonic: String, lang: String)

    case object GetRewardsAddress

    case class UpdateRewardsAddress(address: Address)

  }

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

object ForgerRef {

  def props(settings: AppSettings, appContext: AppContext)(implicit ec: ExecutionContext): Props =
    Props(new Forger(settings, appContext)(ec, appContext.networkType.netPrefix))

  def apply(name: String, settings: AppSettings, appContext: AppContext)(implicit
    system:       ActorSystem,
    ec:           ExecutionContext
  ): ActorRef =
    system.actorOf(props(settings, appContext), name)
}

sealed trait UnlockKeyFailure
case class UnlockKeyFailureException(throwable: Throwable) extends UnlockKeyFailure
sealed trait LockKeyFailure
case class LockKeyFailureException(throwable: Throwable) extends LockKeyFailure
sealed trait CreateKeyFailure
case class CreateKeyFailureException(throwable: Throwable) extends CreateKeyFailure
sealed trait ImportKeyFailure
case class ImportKeyFailureException(throwable: Throwable) extends ImportKeyFailure
sealed trait GetRewardsAddressFailure
case class GetRewardsAddressFailureException(throwable: Throwable) extends GetRewardsAddressFailure
sealed trait UpdateRewardsAddressFailure
case class UpdateRewardsAddressFailureException(throwable: Throwable) extends UpdateRewardsAddressFailure
sealed trait ListOpenKeyfilesFailure
case class ListOpenKeyfilesFailureException(throwable: Throwable) extends ListOpenKeyfilesFailure

trait KeyManagerInterface {
  def unlockKey(address:  String, password: String): EitherT[Future, UnlockKeyFailure, Address]
  def lockKey(address:    Address): EitherT[Future, LockKeyFailure, Done.type]
  def createKey(password: String): EitherT[Future, CreateKeyFailure, Address]
  def importKey(password: String, mnemonic: String, lang: String): EitherT[Future, ImportKeyFailure, Address]
  def getRewardsAddress(): EitherT[Future, GetRewardsAddressFailure, String]
  def updateRewardsAddress(address: Address): EitherT[Future, UpdateRewardsAddressFailure, Done.type]
  def listOpenKeyfiles(): EitherT[Future, ListOpenKeyfilesFailure, Set[Address]]
}

class ActorKeyManagerInterface(actorRef: ActorRef)(implicit ec: ExecutionContext, timeout: Timeout)
    extends KeyManagerInterface {
  import co.topl.akka.CatsActor._

  override def unlockKey(address: String, password: String): EitherT[Future, UnlockKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](Forger.ReceivableMessages.UnlockKey(address, password))
      .leftMap { case AskException(throwable) => UnlockKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(UnlockKeyFailureException))

  override def lockKey(address: Address): EitherT[Future, LockKeyFailure, Done.type] =
    actorRef
      .askEither[Try[_]](Forger.ReceivableMessages.LockKey(address))
      .leftMap { case AskException(throwable) => LockKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(LockKeyFailureException))
      .map(_ => Done)
      .leftMap(e => e: LockKeyFailure)

  override def createKey(password: String): EitherT[Future, CreateKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](Forger.ReceivableMessages.CreateKey(password))
      .leftMap { case AskException(throwable) => CreateKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(CreateKeyFailureException))
      .leftMap(e => e: CreateKeyFailure)

  override def importKey(
    password: String,
    mnemonic: String,
    lang:     String
  ): EitherT[Future, ImportKeyFailure, Address] =
    actorRef
      .askEither[Try[Address]](Forger.ReceivableMessages.ImportKey(password, mnemonic = mnemonic, lang = lang))
      .leftMap { case AskException(throwable) => ImportKeyFailureException(throwable) }
      .subflatMap(_.toEither.leftMap(ImportKeyFailureException))
      .leftMap(e => e: ImportKeyFailure)

  override def getRewardsAddress(): EitherT[Future, GetRewardsAddressFailure, String] =
    actorRef
      .askEither[String](Forger.ReceivableMessages.GetRewardsAddress)
      .leftMap { case AskException(throwable) => GetRewardsAddressFailureException(throwable) }
      .leftMap(e => e: GetRewardsAddressFailure)

  override def updateRewardsAddress(address: Address): EitherT[Future, UpdateRewardsAddressFailure, Done.type] =
    actorRef
      .askEither[String](Forger.ReceivableMessages.UpdateRewardsAddress(address))
      .leftMap { case AskException(throwable) => UpdateRewardsAddressFailureException(throwable) }
      .leftMap(e => e: UpdateRewardsAddressFailure)
      .map(_ => Done)

  override def listOpenKeyfiles(): EitherT[Future, ListOpenKeyfilesFailure, Set[Address]] =
    actorRef
      .askEither[Set[Address]](Forger.ReceivableMessages.ListKeys)
      .leftMap { case AskException(throwable) => ListOpenKeyfilesFailureException(throwable) }
      .leftMap(e => e: ListOpenKeyfilesFailure)
}

object ActorKeyManagerInterface {

  implicit def interface(actorRef: ActorRef)(implicit ec: ExecutionContext, timeout: Timeout): ActorKeyManagerInterface =
    new ActorKeyManagerInterface(actorRef)
}

sealed trait StartForgingFailure
sealed trait StopForgingFailure

trait ForgerInterface {
  def startForging(): EitherT[Future, StartForgingFailure, Done.type]
  def stopForging(): EitherT[Future, StopForgingFailure, Done.type]
}

class ActorForgerInterface(actorRef: ActorRef)(implicit ec: ExecutionContext) extends ForgerInterface {

  override def startForging(): EitherT[Future, StartForgingFailure, Done.type] =
    (actorRef ! Forger.ReceivableMessages.StartForging).asRight[StartForgingFailure].map(_ => Done).toEitherT[Future]

  override def stopForging(): EitherT[Future, StopForgingFailure, Done.type] =
    (actorRef ! Forger.ReceivableMessages.StopForging).asRight[StopForgingFailure].map(_ => Done).toEitherT[Future]
}

object ActorForgerInterface {

  implicit def interface(actorRef: ActorRef)(implicit ec: ExecutionContext): ActorForgerInterface =
    new ActorForgerInterface(actorRef)
}
