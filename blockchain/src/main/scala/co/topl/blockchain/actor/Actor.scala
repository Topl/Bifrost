package co.topl.blockchain.actor

import cats.effect._
import cats.effect.std.Queue
import cats.effect.syntax.all._
import cats.syntax.all._
import co.topl.blockchain.actor.Actor.ActorId
import fs2.Stream
import fs2.concurrent.SignallingRef

import scala.collection.immutable.ListMap
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait Actor[F[_], I, O] {

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

  type FinalizeFiber = Fiber[F, Throwable, Unit]

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
  def releaseActor[I2, O2](actor: Actor[F, I2, O2])(implicit t: Temporal[F]): F[FinalizeFiber]

  def !(input: I): F[Unit] = sendNoWait(input)
}

case class ActorDeadException(msg: String) extends Exception(msg)

object Actor {

  type ActorId = Int

  /**
   * Create a new actor that can self-reference
   *
   * @param initialState the initial state
   * @param createFsm    fsm constructor with self actor ref
   * @param finalize     the finalizer effect with the last state
   * @return Actor object
   */
  def makeFull[F[_]: Concurrent, S, I, O](
    initialState: S,
    createFsm:    Actor[F, I, O] => Fsm[F, S, I, O],
    finalize:     S => F[Unit]
  ): Resource[F, Actor[F, I, O]] =
    for {
      actorRef       <- Deferred[F, Actor[F, I, O]].toResource
      mailbox        <- Queue.unbounded[F, (I, Deferred[F, O])].toResource
      isDeadRef      <- Ref.of[F, Boolean](false).toResource
      acquiredActors <- Ref.of[F, ListMap[ActorId, F[Unit]]](ListMap.empty).toResource
      _ <- Stream
        .eval((Ref.of[F, S](initialState), actorRef.get).tupled)
        .flatMap { case (ref, actorRef) =>
          val fsm = createFsm(actorRef)
          Stream
            .fromQueueUnterminated(mailbox, 1)
            .evalScan(initialState) { case (state, (input, replyTo)) =>
              fsm
                .run(state, input)
                .flatMap { case (newState, output) =>
                  ref.set(newState) *>
                  replyTo.complete(output).as(newState)
                }
            }
            .onFinalize(
              (isDeadRef.set(true) *>
              acquiredActors.get.flatMap(map => map.values.reduceOption(_ *> _).getOrElse(().pure[F])) *>
              ref.get.flatMap(finalize)).uncancelable
            )
        }
        .compile
        .drain
        .background

      throwIfDead = isDeadRef.get.flatMap(Concurrent[F].raiseWhen(_)(ActorDeadException("Actor is dead")))
      actor = new Actor[F, I, O] {
        override val id: Int = mailbox.hashCode()

        override def mailboxSize: F[Int] = mailbox.size

        override def sendNoWait(input: I): F[Unit] =
          throwIfDead *> Deferred[F, O].flatMap(mailbox.offer(input, _)).void

        override def send(msg: I): F[O] =
          throwIfDead *> Deferred[F, O].flatMap(promise => mailbox.offer(msg, promise) *> promise.get)

        override def acquireActor[I2, O2](actorCreator: () => Resource[F, Actor[F, I2, O2]]): F[Actor[F, I2, O2]] =
          for {
            (allocatedActor, finalizer) <- actorCreator().allocated
            _                           <- acquiredActors.update(map => map + (allocatedActor.id -> finalizer))
          } yield allocatedActor

        override def moveActor[I2, O2](actor: Actor[F, I2, O2]): F[Unit] = {
          val actorId = actor.id
          for {
            finalizer <- acquiredActors.get.flatMap(_.getOrElse(actorId, ().pure[F]))
            _         <- acquiredActors.update(map => map - actor.id)
          } yield finalizer
        }

        override def releaseActor[I2, O2](actor: Actor[F, I2, O2])(implicit t: Temporal[F]): F[FinalizeFiber] =
          actor.gracefulShutdown(moveActor(actor))
      }
      _ <- actorRef.complete(actor).toResource
    } yield actor

  /**
   * Create a new actor with finalizer
   *
   * @param initialState Initial state of the actor
   * @param fsm          the finite state machine
   * @param finalize     the cleanup effect with the last known state
   * @return Actor object
   */
  def makeWithFinalize[F[_]: Concurrent, S, I, O](
    initialState: S,
    fsm:          Fsm[F, S, I, O],
    finalize:     S => F[Unit]
  ): Resource[F, Actor[F, I, O]] =
    makeFull(initialState, (_: Actor[F, I, O]) => fsm, finalize)

  def make[F[_]: Concurrent, S, I, O](
    initialState: S,
    fsm:          Fsm[F, S, I, O]
  ): Resource[F, Actor[F, I, O]] =
    makeWithFinalize(initialState, fsm, (_: S) => Concurrent[F].unit)

  def makeSimple[F[_]: Concurrent, S, I, O](
    initialState: S,
    fsm:          Fsm[F, S, I, O]
  ): Resource[F, I => F[O]] =
    make(initialState, fsm).map(_.send)

  implicit class ActorOps[F[_]: Concurrent: Temporal, I, O](actor: Actor[F, I, O]) {

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
