package co.topl.consensus

import akka.Done
import akka.actor.typed.receptionist.{Receptionist, ServiceKey}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import akka.stream.{KillSwitches, Materializer, UniqueKillSwitch}
import akka.util.Timeout
import cats.data.{EitherT, Validated}
import cats.implicits._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.KeyManager.{KeyView, StartupKeyView}
import co.topl.consensus.genesis.{HelGenesis, PrivateGenesis, ToplnetGenesis, ValhallaGenesis}
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, ProgramId}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, Transaction}
import co.topl.nodeView.NodeViewHolderInterface
import co.topl.nodeView.mempool.MemPoolReader
import co.topl.nodeView.state.StateReader
import co.topl.settings.{AppContext, AppSettings}
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType._
import co.topl.utils.{Int128, NetworkType, TimeProvider}
import org.slf4j.Logger

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Try}

private class ForgerBehaviors(
  settings:                AppSettings,
  appContext:              AppContext,
  fetchKeyView:            () => Future[KeyView],
  fetchStartupKeyView:     () => Future[StartupKeyView],
  nodeViewHolderInterface: NodeViewHolderInterface
)(implicit context:        ActorContext[Forger.ReceivableMessage], networkPrefix: NetworkPrefix, timeProvider: TimeProvider) {
  implicit private val mat: Materializer = Materializer(context)
  import context.executionContext
  implicit private val log: Logger = context.log

  import Forger._

  val idle: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartForging(replyTo) =>
        log.info("Starting forging")
        val (killSwitch, streamCompletionFuture) =
          blockForger(
            settings.forging.blockGenerationDelay,
            settings.forging.minTransactionFee,
            fetchKeyView,
            nodeViewHolderInterface
          )
            .run()

        streamCompletionFuture.onComplete {
          case Failure(e) => context.self.tell(ReceivableMessages.Terminate(e))
          case _          =>
        }

        replyTo.tell(Done)
        active(killSwitch, streamCompletionFuture)
      case ReceivableMessages.StopForging(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  private val uninitializedStashSize = 100

  val uninitialized: Behavior[ReceivableMessage] =
    Behaviors.withStash[ReceivableMessage](uninitializedStashSize) { stash =>
      Behaviors.receiveMessage[ReceivableMessage] {
        case m: ReceivableMessages.StartForging =>
          stash.stash(m)
          Behaviors.same
        case m: ReceivableMessages.StopForging =>
          stash.stash(m)
          Behaviors.same
        case ReceivableMessages.NodeViewHolderReady =>
          context.pipeToSelf(checkPrivateForging(appContext, fetchStartupKeyView))(
            _.fold(ReceivableMessages.Terminate, _ => ReceivableMessages.InitializationComplete)
          )
          Behaviors.same
        case ReceivableMessages.InitializationComplete =>
          context.log.info(s"${Console.YELLOW}Forger transitioning to the operational state${Console.RESET}")
          stash.unstashAll(idle)
        case ReceivableMessages.Terminate(reason) =>
          throw reason
      }
    }

  def active(
    killSwitch:       UniqueKillSwitch,
    completionFuture: Future[Done]
  ): Behaviors.Receive[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StopForging(replyTo) =>
        log.info("Stopping forging")
        killSwitch.shutdown()
        completionFuture.onComplete(_ => replyTo.tell(Done))
        idle
      case ReceivableMessages.StartForging(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }
}

object Forger {

  val actorName = "forger"

  val serviceKey: ServiceKey[ReceivableMessage] = ServiceKey[ReceivableMessage](actorName)

  sealed trait ReceivableMessage

  object ReceivableMessages {

    case class StartForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case class StopForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case object NodeViewHolderReady extends ReceivableMessage

    private[consensus] case object InitializationComplete extends ReceivableMessage

    private[consensus] case class Terminate(reason: Throwable) extends ReceivableMessage

  }

  case class ChainParams(totalStake: Int128, difficulty: Long)

  private case object ForgerTick

  def behavior(
    settings:                AppSettings,
    appContext:              AppContext,
    fetchKeyView:            () => Future[KeyView],
    fetchStartupKeyView:     () => Future[StartupKeyView],
    nodeViewHolderInterface: NodeViewHolderInterface
  )(implicit networkPrefix:  NetworkPrefix, timeProvider: TimeProvider): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      context.system.receptionist.tell(Receptionist.Register(serviceKey, context.self))

      protocolMngr = ProtocolVersioner(settings.application.version, settings.forging.protocolVersions)
      consensusStorage = ConsensusStorage(settings, appContext.networkType)

      context.pipeToSelf(nodeViewHolderInterface.onReady())(
        _.fold(ReceivableMessages.Terminate, _ => ReceivableMessages.NodeViewHolderReady)
      )

      new ForgerBehaviors(
        settings,
        appContext,
        fetchKeyView,
        fetchStartupKeyView,
        nodeViewHolderInterface
      ).uninitialized

    }

  def genesisBlock(settings: AppSettings, networkType: NetworkType, fetchStartupKeyView: () => Future[StartupKeyView])(
    implicit ec:             ExecutionContext
  ): Future[Block] = {
    implicit val networkPrefix: NetworkPrefix = networkType.netPrefix

    def initializeFromChainParamsAndGetBlock(block: Try[(Block, ChainParams)]): Try[Block] =
      block.map { case (block: Block, ChainParams(totalStake, initDifficulty)) =>
        consensusStorage.updateConsensusStorage(block.id, ConsensusParams(totalStake, initDifficulty, 0, 0))

        block
      }

    networkType match {
      case Mainnet         => Future.fromTry(initializeFromChainParamsAndGetBlock(ToplnetGenesis.getGenesisBlock))
      case ValhallaTestnet => Future.fromTry(initializeFromChainParamsAndGetBlock(ValhallaGenesis.getGenesisBlock))
      case HelTestnet      => Future.fromTry(initializeFromChainParamsAndGetBlock(HelGenesis.getGenesisBlock))
      case LocalTestnet | PrivateTestnet =>
        fetchStartupKeyView()
          .map(view => PrivateGenesis(view.addresses, settings).getGenesisBlock)
          .flatMap(r => Future.fromTry(initializeFromChainParamsAndGetBlock(r)))
      case _ =>
        Future.failed(new IllegalArgumentException(s"Undefined network type $networkType"))
    }

  }

  private[consensus] def blockForger(
    blockGenerationDelay:    FiniteDuration,
    minTransactionFee:       Int128,
    fetchKeyView:            () => Future[KeyView],
    nodeViewHolderInterface: NodeViewHolderInterface
  )(implicit
    networkPrefix: NetworkPrefix,
    log:           Logger,
    timeProvider:  TimeProvider
  ): RunnableGraph[(UniqueKillSwitch, Future[Done])] =
    Source
      .tick(0.seconds, blockGenerationDelay, ForgerTick)
      .viaMat(KillSwitches.single)(Keep.right)
      .via(
        Flow.fromMaterializer { (mat, _) =>
          import mat.executionContext
          Flow[Any]
            .mapAsync(1)(_ =>
              prepareForgingDependencies(minTransactionFee, fetchKeyView, nodeViewHolderInterface).value
            )
        }
      )
      .map(_.leftMap(LeaderElectionFailure).flatMap(forgeBlockWithBox))
      .recover { case e => Left(ForgingError(e)) }
      .alsoTo(
        Sink.foreach[Either[AttemptForgingFailure, Block]] {
          case Right(block) =>
            log.debug(s"New local block ${block.id} created with parent ${block.parentId} at height ${block.height}")
          case Left(ForgingError(error)) =>
            log.warn("Forger was eligible to forge a new block, but an error occurred.", error)
          case Left(LeaderElectionFailure(LeaderElection.NoAddressesAvailable)) =>
            log.warn("Forger has no addresses available to stake with.")
          case Left(LeaderElectionFailure(LeaderElection.NoBoxesEligible)) =>
            log.debug("No Arbit boxes are eligible at the current difficulty.")
          case Left(LeaderElectionFailure(LeaderElection.NoArbitBoxesAvailable)) =>
            log.debug("No arbit boxes available to stake with.")
        }
      )
      .collect { case Right(v) => LocallyGeneratedModifier(v) }
      .toMat(
        Sink
          .fromMaterializer { case (mat, _) =>
            Sink.foreach(mat.system.eventStream.publish)
          }
          .mapMaterializedValue(_.flatten)
      )(Keep.both)

  private def prepareForgingDependencies(
    minTransactionFee:       Int128,
    fetchKeyView:            () => Future[KeyView],
    nodeViewHolderInterface: NodeViewHolderInterface
  )(implicit
    ec:            ExecutionContext,
    networkPrefix: NetworkPrefix,
    log:           Logger,
    timeProvider:  TimeProvider
  ): EitherT[Future, LeaderElection.IneligibilityReason, Forger.Dependencies] = EitherT {
    for {
      keyView <- fetchKeyView()
      dependencies <- nodeViewHolderInterface
        .withNodeView[Either[LeaderElection.IneligibilityReason, Forger.Dependencies]] { nodeView =>
          val forgeTime = timeProvider.time

          val rewardAddress = keyView.rewardAddr.getOrElse(throw new Error("No rewards address specified"))

          val transactions =
            pickTransactions(
              minTransactionFee,
              nodeView.memPool,
              nodeView.state,
              nodeView.history.height
            ).map(_.toApply).getOrThrow()

          val parentBlock = nodeView.history.bestBlock

          // create the coinbase and unsigned fee reward transactions
          val rewards = Rewards(transactions, rewardAddress, parentBlock.id, forgeTime).getOrThrow()

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
        }
        .valueOrF(e => Future.failed(e.reason))
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
    minTransactionFee:      Int128,
    memPoolReader:          MemPoolReader[Transaction.TX],
    stateReader:            StateReader[ProgramId, Address],
    chainHeight:            Long
  )(implicit networkPrefix: NetworkPrefix, log: Logger): Try[PickTransactionsResult] =
    Try {

      memPoolReader
        .take[Int128](numTxInBlock(chainHeight))(-_.tx.fee) // returns a sequence of transactions ordered by their fee
        .filter(
          _.tx.fee >= minTransactionFee
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
  )(implicit networkPrefix: NetworkPrefix, log: Logger): Either[ForgingError, Block] = {

    // generate the address the owns the generator box
    val matchingAddr = Address(dependencies.box.evidence)

    // lookup the public associated with the box,
    // (this is separate from the signing function so that the private key never leaves the KeyRing)
    val publicKey = dependencies.getPublicKey(matchingAddr).getOrThrow()

    // use the private key that owns the generator box to create a function that will sign the new block
    val signingFunction = dependencies.sign(matchingAddr)

    // use the secret key that owns the successful box to sign the rewards transactions
    val getAttMap: Transaction.TX => Map[PublicKeyPropositionCurve25519, SignatureCurve25519] = tx =>
      signingFunction(tx.messageToSign).map(signature => Map(publicKey -> signature)).getOrThrow()

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
      .leftMap(ForgingError)
  }

  private[consensus] def checkPrivateForging(
    appContext:          AppContext,
    fetchStartupKeyView: () => Future[StartupKeyView]
  )(implicit ec:         ExecutionContext): Future[Done] =
    if (Seq(PrivateTestnet, LocalTestnet).contains(appContext.networkType)) {
      fetchStartupKeyView().flatMap {
        case keyView if keyView.rewardAddr.nonEmpty => Future.successful(Done)
        case _                                      => Future.failed(new IllegalStateException("Forging requires a rewards address"))
      }
    } else {
      Future.successful(Done)
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

private case class PickTransactionsResult(toApply: Seq[Transaction.TX], toEliminate: Seq[Transaction.TX])

sealed private trait AttemptForgingFailure
private case class LeaderElectionFailure(reason: LeaderElection.IneligibilityReason) extends AttemptForgingFailure
private case class ForgingError(error: Throwable) extends AttemptForgingFailure

case class LocallyGeneratedModifier(block: Block)
