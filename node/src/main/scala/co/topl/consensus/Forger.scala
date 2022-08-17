package co.topl.consensus

import akka.Done
import akka.actor.typed.eventstream.EventStream
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import akka.actor.typed.{ActorRef, ActorSystem, Behavior}
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._
import co.topl.consensus.KeyManager.KeyView
import co.topl.modifier.block.Block
import co.topl.nodeView.NodeViewReader
import co.topl.utils.NetworkType._
import co.topl.utils.{Int128, TimeProvider}
import org.slf4j.Logger

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

/**
 * The "Forger" is an Actor which manages and executes a periodic "forge" process.
 */
object Forger {

  val ActorName = "forger"

  sealed abstract class ForgerStatus

  case object Active extends ForgerStatus

  case object Idle extends ForgerStatus

  case object Uninitialized extends ForgerStatus

  sealed trait ReceivableMessage

  object ReceivableMessages {

    case class StartForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case class StopForging(replyTo: ActorRef[Done]) extends ReceivableMessage

    case class CheckForgerStatus(replyTo: ActorRef[ForgerStatus]) extends ReceivableMessage

    private[consensus] case object InitializationComplete extends ReceivableMessage

    private[consensus] case object ForgerTick extends ReceivableMessage

    private[consensus] case class ForgeAttemptComplete(result: Either[ForgerFailure, Block]) extends ReceivableMessage

    private[consensus] case class Terminate(reason: Throwable) extends ReceivableMessage

  }

  case class ChainParams(totalStake: Int128, difficulty: Long)

  def behavior(
    blockGenerationDelay: FiniteDuration,
    minTransactionFee:    Int128,
    forgeOnStartup:       Boolean,
    fetchKeyView:         () => Future[KeyView],
    nodeViewReader:       NodeViewReader
  )(implicit
    networkPrefix:     NetworkPrefix,
    protocolVersioner: ProtocolVersioner,
    timeProvider:      TimeProvider
  ): Behavior[ReceivableMessage] =
    Behaviors.setup { implicit context =>
      import context.executionContext

      if (forgeOnStartup) {
        context.log.info(s"${Console.YELLOW}Forging will start after initialization${Console.RESET}")
      }

      context.pipeToSelf(checkPrivateForging(fetchKeyView))(
        _.fold(ReceivableMessages.Terminate, _ => ReceivableMessages.InitializationComplete)
      )

      new ForgerBehaviors(
        blockGenerationDelay,
        minTransactionFee,
        fetchKeyView,
        nodeViewReader
      ).uninitialized(forgeWhenReady = forgeOnStartup)

    }

  /**
   * If this node is running a private or local network, verify that a rewards address is set
   */
  private def checkPrivateForging(fetchKeyView: () => Future[KeyView])(implicit
    ec:                                         ExecutionContext,
    networkPrefix:                              NetworkPrefix
  ): Future[Done] =
    if (PrivateTestnet.netPrefix == networkPrefix) {
      fetchKeyView().flatMap {
        case keyView if keyView.rewardAddr.nonEmpty => Future.successful(Done)
        case _ => Future.failed(new IllegalStateException("Forging requires a rewards address"))
      }
    } else {
      Future.successful(Done)
    }

  sealed trait ForgerFailure

  case class ForgeFailure(forgeFailure: Forge.Failure) extends ForgerFailure

  case class ForgingError(error: Throwable) extends ForgerFailure

}

private class ForgerBehaviors(
  blockGenerationDelay: FiniteDuration,
  minTransactionFee:    Int128,
  fetchKeyView:         () => Future[KeyView],
  nodeViewReader:       NodeViewReader
)(implicit
  context:           ActorContext[Forger.ReceivableMessage],
  networkPrefix:     NetworkPrefix,
  protocolVersioner: ProtocolVersioner,
  timeProvider:      TimeProvider
) {

  import context.executionContext

  implicit private val log: Logger = context.log

  import Forger._

  /**
   * The actor is initialized and can be instructed to start forging
   */
  val idle: Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartForging(replyTo) =>
        log.info(s"${Console.YELLOW}Starting forging${Console.RESET}")
        replyTo.tell(Done)
        context.self.tell(ReceivableMessages.ForgerTick)
        active
      case ReceivableMessages.StopForging(replyTo) =>
        replyTo.tell(Done)
        Behaviors.same
      case ReceivableMessages.CheckForgerStatus(replyTo) =>
        replyTo.tell(Idle)
        Behaviors.same
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  /**
   * The actor is not yet initialized and is awaiting an asynchronous ready signal
   *
   * @param forgeWhenReady a flag indicating if forging should begin immediately after initialization
   */
  def uninitialized(forgeWhenReady: Boolean): Behavior[ReceivableMessage] =
    Behaviors.receiveMessagePartial[ReceivableMessage] {
      case ReceivableMessages.StartForging(replyTo) =>
        replyTo.tell(Done)
        uninitialized(forgeWhenReady = true)
      case ReceivableMessages.StopForging(replyTo) =>
        replyTo.tell(Done)
        uninitialized(forgeWhenReady = false)
      case ReceivableMessages.CheckForgerStatus(replyTo) =>
        replyTo.tell(Uninitialized)
        Behaviors.same
      case ReceivableMessages.InitializationComplete =>
        context.log.info(s"${Console.YELLOW}Forger is initialized${Console.RESET}")
        if (forgeWhenReady) context.self.tell(ReceivableMessages.StartForging(context.system.ignoreRef))
        idle
      case ReceivableMessages.Terminate(reason) =>
        throw reason
    }

  /**
   * The forger is actively generating new blocks and can be instructed to stop
   */
  val active: Behavior[ReceivableMessage] =
    Behaviors.withTimers[ReceivableMessage] { scheduler =>
      Behaviors.receiveMessagePartial[ReceivableMessage] {
        case ReceivableMessages.StopForging(replyTo) =>
          log.info(s"${Console.YELLOW}Stopping forging${Console.RESET}")
          replyTo.tell(Done)
          scheduler.cancelAll()
          idle
        case ReceivableMessages.StartForging(replyTo) =>
          replyTo.tell(Done)
          Behaviors.same
        case ReceivableMessages.CheckForgerStatus(replyTo) =>
          replyTo.tell(Active)
          Behaviors.same
        case ReceivableMessages.ForgerTick =>
          context.pipeToSelf(nextBlock().value)(result =>
            ReceivableMessages.ForgeAttemptComplete(result.toEither.leftMap(ForgingError).flatten)
          )
          Behaviors.same
        case ReceivableMessages.ForgeAttemptComplete(result) =>
          logResult(result)
          result.foreach(block => context.system.eventStream.tell(EventStream.Publish(LocallyGeneratedBlock(block))))
          result match {
            case Left(ForgeFailure(Forge.NoRewardsAddressSpecified)) |
                Left(ForgeFailure(Forge.LeaderElectionFailure(NxtLeaderElection.NoAddressesAvailable))) =>
              // In these specific cases, it would not help to try again
              scheduler.cancelAll()
              log.info("Forger transitioning to idle state")
              idle
            case _ =>
              scheduler.startSingleTimer(ReceivableMessages.ForgerTick, blockGenerationDelay)
              Behaviors.same
          }
        case ReceivableMessages.Terminate(reason) =>
          scheduler.cancelAll()
          throw reason
      }
    }

  /**
   * Forge the "next" block based on the "current" NodeView
   */
  private def nextBlock(): EitherT[Future, ForgerFailure, Block] =
    for {
      keyView <- EitherT[Future, ForgerFailure, KeyView](
        fetchKeyView().map(Right(_)).recover { case e => Left(ForgingError(e)) }
      )
      forge <- nodeViewReader
        .withNodeView(Forge.prepareForge(_, keyView, minTransactionFee))
        .leftMap(e => ForgingError(e.reason))
        .subflatMap(_.leftMap(ForgeFailure(_): ForgerFailure))
      block <- EitherT.fromEither[Future](forge.make).leftMap(ForgeFailure(_): ForgerFailure)
    } yield block

  /**
   * Log the result of a forging attempt
   */
  private def logResult(result: Either[ForgerFailure, Block]): Unit =
    result match {
      case Right(block) =>
        log.debug(s"New local block ${block.id} created with parent ${block.parentId} at height ${block.height}")
      case Left(ForgingError(error)) =>
        log.warn("Forger was eligible to forge a new block, but an error occurred.", error)
      case Left(ForgeFailure(f)) =>
        f match {
          case Forge.LeaderElectionFailure(reason) =>
            reason match {
              case NxtLeaderElection.NoAddressesAvailable =>
                log.warn("Forger has no addresses available to stake with.")
              case NxtLeaderElection.NoBoxesEligible =>
                log.debug("No Arbit boxes are eligible at the current difficulty.")
              case NxtLeaderElection.NoArbitBoxesAvailable =>
                log.debug("No arbit boxes available to stake with.")
            }
          case Forge.NoRewardsAddressSpecified =>
            log.debug("No rewards address was specified.  Stopping forging.")
          case Forge.ForgingError(error) =>
            log.warn("Forger was eligible to forge a new block, but an error occurred.", error)
          case Forge.ArbitBoxKeyNotFound =>
            log.warn("Key for selected ArbitBox not found in KeyRing.")
        }
    }
}

trait ForgerInterface {

  /**
   * Instruct the Forger to start forging.  This is an asynchronous signal.  Forging
   * may not begin immediately.  A successful response indicates the Forger received and acknowledges the signal.
   */
  def startForging(): EitherT[Future, ForgerInterface.StartForgingFailure, Done]

  /**
   * Instruct the Forger to stop forging.  This is an asynchronous signal.  Forging
   * may not stop immediately.  A successful response indicates the Forger received and acknowledges the signal.
   */
  def stopForging(): EitherT[Future, ForgerInterface.StopForgingFailure, Done]

  /**
   * Check the status of the node. A successful response indicates the status(eg. forging status) of the forger.
   */
  def checkForgerStatus(): EitherT[Future, ForgerInterface.CheckStatusFailure, Forger.ForgerStatus]
}

object ForgerInterface {

  sealed trait StartForgingFailure

  sealed trait StopForgingFailure

  sealed trait CheckStatusFailure
}

class ActorForgerInterface(actorRef: ActorRef[Forger.ReceivableMessage])(implicit system: ActorSystem[_])
    extends ForgerInterface {

  import system.executionContext

  implicit private val timeout: Timeout = Timeout(10.minutes)

  import akka.actor.typed.scaladsl.AskPattern._

  override def startForging(): EitherT[Future, ForgerInterface.StartForgingFailure, Done] =
    EitherT.liftF(actorRef.ask[Done](Forger.ReceivableMessages.StartForging))

  override def stopForging(): EitherT[Future, ForgerInterface.StopForgingFailure, Done] =
    EitherT.liftF(actorRef.ask[Done](Forger.ReceivableMessages.StopForging))

  override def checkForgerStatus(): EitherT[Future, ForgerInterface.CheckStatusFailure, Forger.ForgerStatus] =
    EitherT.liftF(actorRef.ask[Forger.ForgerStatus](Forger.ReceivableMessages.CheckForgerStatus))
}

/**
 * A broadcastable signal indicating that a new block was forged locally
 *
 * @param block The new block that was forged
 */
case class LocallyGeneratedBlock(block: Block)
