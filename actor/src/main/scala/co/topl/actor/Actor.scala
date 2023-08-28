package co.topl.actor

import cats.Applicative
import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all._
import cats.syntax.all._
import co.topl.actor.Actor.ActorId
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.collection.immutable.ListMap
import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
 * Fs2 actor, inspired by https://gist.github.com/Swoorup/1ac9b69e0c0f1c0925d1397a94b0a762
 * @tparam F type of effect
 * @tparam I type for actor incoming message
 * @tparam O type for actor response
 */
trait Actor[F[_], I, O] {

  /**
   * Name for current actor
   * @return
   */
  def name: String

  /**
   * Get unique actor id
   * @return
   */
  def id: ActorId

  /**
   * Get number unprocessed messages for that actor
   */
  def mailboxSize: F[Int]

  /**
   * Send the message without waiting for acknowledgement
   *
   * @param msg the message send
   */
  def sendNoWait(msg: I): F[Unit]

  /**
   * Send the message and wait for acknowledgement with output
   *
   * @param msg the message to send
   */
  def send(msg: I): F[O]

  /**
   * Create and bind new actor to the current actor, new actor will be gracefully shutdown,
   * i.e. his finalize function will be called, if current actor will be finished
   * @param actorCreator function for new child actor creation
   * @tparam I2 type of input parameter for child actor
   * @tparam O2 type of output parameter for child actor
   * @return allocated and bound actor
   */
  def acquireActor[I2, O2](actorCreator: () => Resource[F, Actor[F, I2, O2]]): F[Actor[F, I2, O2]]

  /**
   * Unbound actor, i.e. child actor finalizer no longer will be called if current actor will be shutdown
   * @param actor actor to move
   * @tparam I2 type of input parameter for child actor
   * @tparam O2 type of output parameter for child actor
   * @return finalizer for moved actor
   */
  def moveActor[I2, O2](actor: Actor[F, I2, O2]): F[Unit]

  type FinalizeFiber[T[_]] = Fiber[T, Throwable, Unit]

  /**
   * Try to shutdown child actor by calling finalize function for that actor if incoming messages queue is empty.
   * Calling that function does not prevent to receive new incoming messages
   * to child actor, thus actor will not shutdown if new messages rate is equal or more processing message rate
   * @param actor actor to release
   * @param t type class for supporting Temporal functionality
   * @tparam I2 type of input parameter for child actor
   * @tparam O2 type of output parameter for child actor
   * @return fin
   */
  def releaseActor[I2, O2](actor: Actor[F, I2, O2])(implicit t: Temporal[F]): F[FinalizeFiber[F]]
}

case class ActorDeadException(msg: String) extends Exception(msg)

object Actor {

  type ActorId = Int

  /**
   * Create a new actor that can self-reference
   *
   * @param actorName name for actor
   * @param initialState the initial state
   * @param createFsm    fsm constructor with self actor ref
   * @param finalize     the finalizer effect with the last state
   * @return Actor object
   */
  def makeFull[F[_]: Concurrent, S, I, O](
    actorName:    String,
    initialState: S,
    createFsm:    Actor[F, I, O] => Fsm[F, S, I, O],
    finalize:     S => F[Unit]
  ): Resource[F, Actor[F, I, O]] =
    for {
      actorRef  <- Deferred[F, Actor[F, I, O]].toResource
      mailbox   <- Queue.unbounded[F, Option[(I, Deferred[F, O])]].toResource
      isDeadRef <- Resource.make(Ref.of[F, Boolean](false))(_.set(true))
      acquiredActors <-
        Resource.make(Ref.of[F, ListMap[ActorId, F[Unit]]](ListMap.empty))(_.get.flatMap(_.values.toList.sequence).void)
      stateRef <- Resource.make(Ref.of[F, S](initialState))(_.get.flatMap(finalize))
      processOutcome <- Stream
        .eval(actorRef.get)
        .flatMap { actorRef =>
          val fsm = createFsm(actorRef)
          Stream
            .fromQueueNoneTerminated(mailbox, 1)
            .evalScan(initialState) { case (state, (input, replyTo)) =>
              fsm
                .run(state, input)
                .flatMap { case (newState, output) =>
                  stateRef.set(newState) *>
                  replyTo.complete(output).as(newState)
                }
            }
        }
        .compile
        .drain
        .background

      _ <- Resource.onFinalize(mailbox.offer(none) *> processOutcome.flatMap(_.embed(Applicative[F].unit)))

      throwIfDead = isDeadRef.get.flatMap(Concurrent[F].raiseWhen(_)(ActorDeadException(s"Actor: $actorName; is dead")))
      actor = new Actor[F, I, O] {
        override val name: String = actorName

        override val id: Int = java.util.Objects.hash(mailbox)

        override def mailboxSize: F[Int] = mailbox.size

        override def sendNoWait(input: I): F[Unit] =
          throwIfDead *> Deferred[F, O].flatMap(promise => mailbox.offer((input, promise).some)).void

        override def send(msg: I): F[O] =
          throwIfDead *> Deferred[F, O].flatMap(promise => mailbox.offer((msg, promise).some) *> promise.get)

        override def acquireActor[I2, O2](actorCreator: () => Resource[F, Actor[F, I2, O2]]): F[Actor[F, I2, O2]] =
          for {
            (allocatedActor, finalizer) <- actorCreator().allocated
            _                           <- acquiredActors.update(map => map + (allocatedActor.id -> finalizer))
          } yield allocatedActor

        override def moveActor[I2, O2](actor: Actor[F, I2, O2]): F[Unit] = {
          val actorId = actor.id
          for {
            finalizer <- acquiredActors.get.flatMap(_.getOrElse(actorId, ().pure[F]))
            _         <- acquiredActors.update(map => map - actorId)
          } yield finalizer
        }

        override def releaseActor[I2, O2](actor: Actor[F, I2, O2])(implicit t: Temporal[F]): F[FinalizeFiber[F]] =
          actor.gracefulShutdown(moveActor(actor))
      }
      _ <- actorRef.complete(actor).toResource
    } yield actor

  /**
   * Create a new actor with finalizer
   *
   * @param actorName name for actor
   * @param initialState Initial state of the actor
   * @param fsm          the finite state machine
   * @param finalize     the cleanup effect with the last known state
   * @return Actor object
   */
  def makeWithFinalize[F[_]: Concurrent, S, I, O](
    actorName:    String,
    initialState: S,
    fsm:          Fsm[F, S, I, O],
    finalize:     S => F[Unit]
  ): Resource[F, Actor[F, I, O]] =
    makeFull(actorName, initialState, (_: Actor[F, I, O]) => fsm, finalize)

  def make[F[_]: Concurrent, S, I, O](
    actorName:    String,
    initialState: S,
    fsm:          Fsm[F, S, I, O]
  ): Resource[F, Actor[F, I, O]] =
    makeWithFinalize(actorName, initialState, fsm, (_: S) => Concurrent[F].unit)

  def makeSimple[F[_]: Concurrent, S, I, O](
    actorName:    String,
    initialState: S,
    fsm:          Fsm[F, S, I, O]
  ): Resource[F, I => F[O]] =
    make(actorName, initialState, fsm).map(_.send)

  implicit class ActorOps[F[_]: Concurrent: Temporal, I, O](actor: Actor[F, I, O]) {

    /**
     * Try to shutdown actor by using given shutdownFunction after attemptTimeout time and every attemptTimeout after
     * @param shutdownFunction function for shutdown actor
     * @param attemptTimeout timeout between attempts
     * @return fiber for shutdown attempt
     */
    def gracefulShutdown(
      shutdownFunction: F[Unit],
      attemptTimeout:   FiniteDuration = 1 second
    ): F[Fiber[F, Throwable, Unit]] = {
      def tryToShutdown(stopSignal: SignallingRef[F, Boolean]) = actor.mailboxSize.flatMap {
        case 0 => shutdownFunction *> stopSignal.set(true)
        case _ => ().pure
      }

      for {
        cancel <- SignallingRef[F, Boolean](false)
        fiber <- Stream
          .awakeDelay(attemptTimeout)
          .evalMap(_ => tryToShutdown(cancel))
          .interruptWhen(cancel)
          .compile
          .drain
          .start
      } yield fiber
    }
  }
}
