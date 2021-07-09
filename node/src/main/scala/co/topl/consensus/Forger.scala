package co.topl.consensus

import akka.actor.ClassicActorContextProvider
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.{ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior, Scheduler}
import akka.pattern.ask
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import akka.util.Timeout
import akka.{Done, NotUsed}
import cats.data.{EitherT, Validated}
import cats.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.KeyManager.ReceivableMessages._
import co.topl.consensus.KeyManager.{AttemptForgingKeyView, ForgerStartupKeyView}
import co.topl.consensus.genesis.{HelGenesis, PrivateGenesis, ToplnetGenesis, ValhallaGenesis}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewChanged, NodeViewHolder}
import co.topl.settings.{AppContext, AppSettings, NodeViewReady}
import co.topl.utils.NetworkType._
import co.topl.utils.{EventStreamSupport, Int128, NetworkType, TimeProvider}
import org.slf4j.Logger

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

object Forger {

  val actorName = "forger"

  val ForgerServiceKey: ServiceKey[ReceivableMessage] = ServiceKey[ReceivableMessage](actorName)

  case class ChainParams(totalStake: Int128, difficulty: Long)

  case class PickTransactionsResult(toApply: Seq[Transaction.TX], toEliminate: Seq[Transaction.TX])

  sealed trait AttemptForgingFailure
  case class LeaderElectionFailure(reason: LeaderElection.IneligibilityReason) extends AttemptForgingFailure
  case class ForgingError(error: Throwable) extends AttemptForgingFailure

  sealed trait ReceivableMessage

  object ReceivableMessages {

    case class GenerateGenesis(replyTo: ActorRef[Try[Block]]) extends ReceivableMessage

    case class StartForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case class StopForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case class NodeViewHolderReady(nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage])
        extends ReceivableMessage

    private[Forger] case class ForgerStreamCompleted(result: Try[_]) extends ReceivableMessage

  }

  def behavior(
    settings:               AppSettings,
    appContext:             AppContext,
    keyManager:             akka.actor.ActorRef
  )(implicit networkPrefix: NetworkPrefix): Behavior[ReceivableMessage] =
    Behaviors.setup { context =>
      import context.executionContext

      implicit val log: Logger = context.log

      implicit val systemProvider: ActorContext[ReceivableMessage] = context

      context.system.receptionist.tell(Receptionist.Register(ForgerServiceKey, context.self))

      protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)
      consensusStorage = ConsensusStorage(settings, appContext.networkType)
      context.system.eventStream.tell(
        EventStream.Subscribe[NodeViewReady](
          context.messageAdapter(m => ReceivableMessages.NodeViewHolderReady(m.nodeViewHolderRef))
        )
      )

      Behaviors.withStash(100)(uninitialized(settings, appContext, keyManager, _))
    }

  private def uninitialized(
    settings:   AppSettings,
    appContext: AppContext,
    keyManager: akka.actor.ActorRef,
    stash:      StashBuffer[ReceivableMessage]
  )(implicit
    ec:                          ExecutionContext,
    networkPrefix:               NetworkPrefix,
    log:                         Logger,
    classicActorContextProvider: ClassicActorContextProvider,
    context:                     ActorContext[ReceivableMessage]
  ): Behaviors.Receive[ReceivableMessage] = {
    implicit val scheduler: Scheduler = context.system.scheduler
    implicit val timeout: Timeout = 5.seconds
    Behaviors.receiveMessage[ReceivableMessage] {
      case ReceivableMessages.GenerateGenesis(replyTo) =>
        generateGenesis(settings, appContext.networkType, keyManager, replyTo)
        Behaviors.same
      case m: ReceivableMessages.StartForging =>
        stash.stash(m)
        Behaviors.same
      case m: ReceivableMessages.StopForging =>
        stash.stash(m)
        Behaviors.same
      case ReceivableMessages.NodeViewHolderReady(ref) =>
        log.info(s"${Console.YELLOW}Forger transitioning to the operational state${Console.RESET}")
        checkPrivateForging(settings, appContext, keyManager, context.self)
        stash.unstashAll(idle(settings, appContext, keyManager, ref))
    }
  }

  private def idle(
    settings:          AppSettings,
    appContext:        AppContext,
    keyManager:        akka.actor.ActorRef,
    nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit
    ec:            ExecutionContext,
    networkPrefix: NetworkPrefix,
    log:           Logger,
    context:       ActorContext[ReceivableMessage]
  ): Behaviors.Receive[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.GenerateGenesis(replyTo) =>
        generateGenesis(settings, appContext.networkType, keyManager, replyTo)
        Behaviors.same
      case ReceivableMessages.StartForging(replyTo) =>
        log.info("Starting forging")
        implicit val mat: Materializer = Materializer(context)
        blockForger(settings, appContext, nodeViewHolderRef, keyManager)
          .run()
          .onComplete(result => context.self.tell(ReceivableMessages.ForgerStreamCompleted(result)))
        replyTo.tell(Done)
        active(mat, settings, appContext, keyManager, nodeViewHolderRef)
    }

  private def active(
    materializer:      Materializer,
    settings:          AppSettings,
    appContext:        AppContext,
    keyManager:        akka.actor.ActorRef,
    nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit
    ec:            ExecutionContext,
    networkPrefix: NetworkPrefix,
    log:           Logger,
    context:       ActorContext[ReceivableMessage]
  ): Behaviors.Receive[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StopForging(replyTo) =>
        log.info("Stopping forging")
        materializer.shutdown()
        replyTo.tell(Done)
        idle(settings, appContext, keyManager, nodeViewHolderRef)
      case ReceivableMessages.ForgerStreamCompleted(result) =>
        result match {
          case Success(_) =>
            throw new IllegalStateException("Forger stream ended unexpectedly")
          case Failure(reason) =>
            log.error("Forging process failed", reason)
            throw reason
        }
    }

  /**
   * Return the correct genesis parameters for the chosen network.
   * NOTE: the default private network is set in AppContext so the fall-through should result in an error.
   */
  private def generateGenesis(
    settings:    AppSettings,
    networkType: NetworkType,
    keyManager:  akka.actor.ActorRef,
    replyTo:     ActorRef[Try[Block]]
  )(implicit ec: ExecutionContext, timeout: Timeout = 10 seconds): Unit = {
    implicit val networkPrefix: NetworkPrefix = networkType.netPrefix
    def generatePrivateGenesis(): Future[Try[(Block, ChainParams)]] =
      (keyManager ? GenerateInitialAddresses)
        .mapTo[Try[ForgerStartupKeyView]]
        .map {
          case Success(view) => PrivateGenesis(view.addresses, settings).getGenesisBlock
          case Failure(ex) =>
            throw new Error("Unable to generate genesis block, no addresses generated.", ex)
        }

    def initializeFromChainParamsAndGetBlock(block: Try[(Block, ChainParams)]): Try[Block] =
      block.map { case (block: Block, ChainParams(totalStake, initDifficulty)) =>
        consensusStorage.updateConsensusStorage(block.id, ConsensusParams(totalStake, initDifficulty, 0, 0))

        block
      }

    networkType match {
      case Mainnet         => replyTo.tell(initializeFromChainParamsAndGetBlock(ToplnetGenesis.getGenesisBlock))
      case ValhallaTestnet => replyTo.tell(initializeFromChainParamsAndGetBlock(ValhallaGenesis.getGenesisBlock))
      case HelTestnet      => replyTo.tell(initializeFromChainParamsAndGetBlock(HelGenesis.getGenesisBlock))
      case LocalTestnet | PrivateTestnet =>
        generatePrivateGenesis()
          .map(initializeFromChainParamsAndGetBlock)
          .onComplete(t => replyTo.tell(t.flatten))
      case _ =>
        throw new Error("Undefined network type.")
    }
  }

  private def blockForger(
    settings:               AppSettings,
    appContext:             AppContext,
    nodeViewHolderRef:      ActorRef[NodeViewHolder.ReceivableMessage],
    keyManagerRef:          akka.actor.ActorRef
  )(implicit networkPrefix: NetworkPrefix, log: Logger): RunnableGraph[Future[Done]] = {

    implicit val timeout: Timeout = settings.forging.blockGenerationDelay
    import EventStreamSupport._
    Source
      .tick(0.seconds, settings.forging.blockGenerationDelay, NotUsed)
      .zip(Source.single(NodeViewChanged).concat(eventStreamSource(NodeViewChanged.getClass)))
      .via(
        Flow.fromMaterializer { (mat, _) =>
          import akka.actor.typed.scaladsl.adapter._
          implicit val system: ActorSystem[_] = mat.system.toTyped
          Flow[Any]
            .mapAsync(1)(_ => prepareForgingDependencies(settings, appContext, keyManagerRef, nodeViewHolderRef).value)
        }
      )
      .map(_.leftMap(Forger.LeaderElectionFailure).flatMap(forgeBlockWithBox))
      .alsoTo(
        Sink.foreach[Either[AttemptForgingFailure, Block]] {
          case Right(block) =>
            log.debug(s"New local block ${block.id} created with parent ${block.parentId} at height ${block.height}")
          case Left(Forger.ForgingError(error)) =>
            log.warn("Forger was eligible to forge a new block, but an error occurred.", error)
          case Left(Forger.LeaderElectionFailure(LeaderElection.NoAddressesAvailable)) =>
            log.warn("Forger has no addresses available to stake with.")
          case Left(Forger.LeaderElectionFailure(LeaderElection.NoBoxesEligible)) =>
            log.debug("No boxes were eligible to forge with.")
          case Left(Forger.LeaderElectionFailure(LeaderElection.NoArbitBoxesAvailable)) =>
            log.debug("No arbit boxes available to stake with.")
        }
      )
      .collect { case Right(v) => LocallyGeneratedModifier(v) }
      .toMat(eventStreamSink)(Keep.right)
  }

  private def prepareForgingDependencies(
    settings:          AppSettings,
    appContext:        AppContext,
    keyManager:        akka.actor.ActorRef,
    nodeViewHolderRef: ActorRef[NodeViewHolder.ReceivableMessage]
  )(implicit
    system:        ActorSystem[_],
    timeout:       Timeout,
    networkPrefix: NetworkPrefix,
    log:           Logger
  ): EitherT[Future, LeaderElection.IneligibilityReason, Forger.Dependencies] = EitherT {
    import system.executionContext
    for {
      keyView <- (keyManager ? GetAttemptForgingKeyView).mapTo[AttemptForgingKeyView]
      dependencies <-
        nodeViewHolderRef
          .ask[Either[LeaderElection.IneligibilityReason, Forger.Dependencies]](
            NodeViewHolder.ReceivableMessages
              .Read(
                { nodeView =>
                  val forgeTime = appContext.timeProvider.time

                  val rewardAddress = keyView.rewardAddr.getOrElse(throw new Error("No rewards address specified"))

                  // pick the transactions from the mempool for inclusion in the block (if successful)
                  val transactions =
                    pickTransactions(settings, nodeView.memPool, nodeView.state, nodeView.history.height) match {
                      case Success(res) =>
                        if (res.toEliminate.nonEmpty)
                          nodeViewHolderRef ! NodeViewHolder.ReceivableMessages
                            .EvictTransactions(res.toEliminate.map(_.id))
                        res.toApply

                      case Failure(ex) => throw ex
                    }

                  val parentBlock = nodeView.history.bestBlock

                  // create the coinbase and unsigned fee reward transactions
                  val rewards = Rewards(transactions, rewardAddress, parentBlock.id, forgeTime) match {
                    case Success(r)  => r
                    case Failure(ex) => throw ex
                  }

                  // retrieve the latest TWO block times for updating the difficulty if we forge a new blow
                  val prevTimes = nodeView.history.getTimestampsFrom(parentBlock, nxtBlockNum)

                  LeaderElection
                    .getEligibleBox(parentBlock, keyView.addresses, forgeTime, nodeView.state)
                    .map(arbitBox =>
                      Forger.Dependencies(
                        arbitBox,
                        parentBlock,
                        prevTimes,
                        rewards,
                        transactions,
                        forgeTime,
                        keyView.sign,
                        keyView.getPublicKey
                      )
                    )
                },
                _
              )
          )
    } yield dependencies
  }

  /**
   * Pick a set of transactions from the mempool that result in a valid state when applied to the current state
   *
   * @param memPoolReader the set of pending transactions
   * @param stateReader state to use for semantic validity checking
   * @return a sequence of valid transactions
   */
  private def pickTransactions(
    settings:               AppSettings,
    memPoolReader:          MemPoolReader[Transaction.TX],
    stateReader:            StateReader[ProgramId, Address],
    chainHeight:            Long
  )(implicit networkPrefix: NetworkPrefix, log: Logger): Try[PickTransactionsResult] =
    Try {

      memPoolReader
        .take[Int128](numTxInBlock(chainHeight))(-_.tx.fee) // returns a sequence of transactions ordered by their fee
        .filter(
          _.tx.fee >= settings.forging.minTransactionFee
        ) // default strategy ignores zero fee transactions in mempool
        .foldLeft(PickTransactionsResult(Seq(), Seq())) { case (txAcc, utx) =>
          // ensure that each transaction opens a unique box by checking that this transaction
          // doesn't open a box already being opened by a previously included transaction
          val boxAlreadyUsed = utx.tx.boxIdsToOpen.exists(id => txAcc.toApply.flatMap(_.boxIdsToOpen).contains(id))

          // if any newly created box matches a box already in the UTXO set in state, remove the transaction
          val boxAlreadyExists = utx.tx.newBoxes.exists(b => stateReader.getBox(b.id).isDefined)

          (boxAlreadyUsed, boxAlreadyExists) match {
            case (false, false) =>
              import co.topl.modifier.transaction.validation.implicits._
              utx.tx.semanticValidation(stateReader) match {
                case Validated.Valid(_) => PickTransactionsResult(txAcc.toApply :+ utx.tx, txAcc.toEliminate)
                case Validated.Invalid(ex) =>
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

  /**
   * Forges a block with the given eligible arbit box and state parameters.
   * @return a block if forging was successful and None otherwise
   */
  private def forgeBlockWithBox(
    dependencies:           Forger.Dependencies
  )(implicit networkPrefix: NetworkPrefix, log: Logger): Either[Forger.ForgingError, Block] = {

    // generate the address the owns the generator box
    val matchingAddr = Address(dependencies.box.evidence)

    // lookup the public associated with the box,
    // (this is separate from the signing function so that the private key never leaves the KeyRing)
    val publicKey = dependencies.getPublicKey(matchingAddr) match {
      case Success(pk) => pk
      case Failure(error) =>
        log.warn("Error occurred while getting public key for address.")
        throw error
    }

    // use the private key that owns the generator box to create a function that will sign the new block
    val signingFunction = dependencies.sign(matchingAddr)

    // use the secret key that owns the successful box to sign the rewards transactions
    val getAttMap: Transaction.TX => Map[PublicKeyPropositionCurve25519, SignatureCurve25519] = (tx: Transaction.TX) =>
      {
        val sig = signingFunction(tx.messageToSign) match {
          case Success(sig) => sig
          case Failure(ex)  => throw ex
        }
        Map(publicKey -> sig)
      }

    val signedRewards = dependencies.rawRewards.map {
      case tx: ArbitTransfer[_] => tx.copy(attestation = getAttMap(tx))
      case tx: PolyTransfer[_]  => tx.copy(attestation = getAttMap(tx))
    }

    // calculate the newly forged blocks updated difficulty
    val newDifficulty = calcNewBaseDifficulty(
      dependencies.parent.height + 1,
      dependencies.parent.difficulty,
      dependencies.previousBlockTimes :+ dependencies.forgeTime
    )

    // add the signed coinbase transaction to the block, sign it, and return the newly forged block
    Block
      .createAndSign(
        dependencies.parent.id,
        dependencies.forgeTime,
        signedRewards ++ dependencies.transactionsToInclude,
        dependencies.box,
        publicKey,
        dependencies.parent.height + 1,
        newDifficulty,
        blockVersion(dependencies.parent.height + 1)
      )(signingFunction)
      .toEither
      .leftMap(Forger.ForgingError)
  }

  private def checkPrivateForging(
    settings:    AppSettings,
    appContext:  AppContext,
    keyManager:  akka.actor.ActorRef,
    self:        ActorRef[ReceivableMessage]
  )(implicit ec: ExecutionContext, timeout: Timeout, log: Logger, scheduler: Scheduler): Unit =
    if (Seq(PrivateTestnet, LocalTestnet).contains(appContext.networkType)) {
      (keyManager ? GenerateInitialAddresses)
        .mapTo[Try[ForgerStartupKeyView]]
        .map {
          case Success(ForgerStartupKeyView(_, Some(_))) =>
            // if forging has been enabled, then we should send the StartForging signal
            if (settings.forging.forgeOnStartup) {
              self.ask(ReceivableMessages.StartForging)
            }

          case Success(ForgerStartupKeyView(_, None)) =>
            log.warn("Forging not started: no reward address set.")

          case _ =>
            log.warn("Forging not started: failed to generate initial addresses in Key Ring.")
        }
        .recover { case ex =>
          log.warn("Forging not started: failed to generate initial addresses in Key Ring: ", ex)
        }
    }

  /**
   * @param box an eligible arbit box
   * @param parent the parent block
   * @param previousBlockTimes the previous block times to determine next difficulty
   * @param rawRewards the raw forging rewards
   * @param transactionsToInclude the set of transactions to be entered into the block
   * @param forgeTime the current timestamp
   * @param sign a function for signing messages
   * @param getPublicKey a function for getting the public key associated with an address
   */
  case class Dependencies(
    box:                   ArbitBox,
    parent:                Block,
    previousBlockTimes:    Vector[TimeProvider.Time],
    rawRewards:            Seq[Transaction.TX],
    transactionsToInclude: Seq[Transaction.TX],
    forgeTime:             TimeProvider.Time,
    sign:                  Address => Array[Byte] => Try[SignatureCurve25519],
    getPublicKey:          Address => Try[PublicKeyPropositionCurve25519]
  )

}

////////////////////////////////////////////////////////////////////////////////////
//////////////////////////////// ACTOR REF HELPER //////////////////////////////////

sealed trait StartForgingFailure
sealed trait StopForgingFailure

trait ForgerInterface {
  def startForging(): EitherT[Future, StartForgingFailure, Done]
  def stopForging(): EitherT[Future, StopForgingFailure, Done]
}

class ActorForgerInterface(actorRef: ActorRef[Forger.ReceivableMessage])(implicit system: ActorSystem[_])
    extends ForgerInterface {
  import system.executionContext

  implicit private val timeout: Timeout = Timeout(10.minutes)

  import akka.actor.typed.scaladsl.AskPattern._

  override def startForging(): EitherT[Future, StartForgingFailure, Done] =
    EitherT.liftF(actorRef.ask[Done](Forger.ReceivableMessages.StartForging))

  override def stopForging(): EitherT[Future, StopForgingFailure, Done] =
    EitherT.liftF(actorRef.ask[Done](Forger.ReceivableMessages.StopForging))
}

case class LocallyGeneratedModifier(block: Block)
