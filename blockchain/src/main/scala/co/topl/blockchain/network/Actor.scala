package co.topl.blockchain.network

import cats.syntax.all._
import cats.effect.std.Queue
import cats.effect.{Concurrent, Deferred, Ref, Resource}
import cats.effect.syntax.all._
import fs2.Stream
import co.topl.blockchain.network.Fsm

trait AskMsg[F[_], R] {
  def replyTo: Deferred[F, R]
}

trait Actor[F[_], I, O] {

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

  def !(input: I): F[Unit] = sendNoWait(input)
}

case class ActorDeadException(msg: String) extends Exception(msg)

object Actor {

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
      actorRef  <- Deferred[F, Actor[F, I, O]].toResource
      mailbox   <- Queue.unbounded[F, (I, Deferred[F, O])].toResource
      isDeadRef <- Ref.of[F, Boolean](false).toResource
      _ <- Stream
        .eval((Ref.of[F, S](initialState), actorRef.get).tupled)
        .flatMap { case (ref, actorRef) =>
          val fsm = createFsm(actorRef)
          Stream
            .fromQueueUnterminated(mailbox)
            .evalScan(initialState) { case (state, (input, replyTo)) =>
              fsm
                .run(state, input)
                .flatMap { case (newState, output) => ref.set(newState) *> replyTo.complete(output).as(newState) }
            }
            .onFinalize((isDeadRef.set(true) *> ref.get.flatMap(finalize)).uncancelable)
        }
        .compile
        .drain
        .background

      throwIfDead = isDeadRef.get.flatMap(Concurrent[F].raiseWhen(_)(ActorDeadException("Actor is dead")))
      actor = new Actor[F, I, O] {
        def sendNoWait(input: I): F[Unit] =
          throwIfDead *> Deferred[F, O].flatMap(mailbox.offer(input, _)).void

        def send(msg: I): F[O] =
          throwIfDead *> Deferred[F, O].flatMap(promise => mailbox.offer(msg, promise) *> promise.get)
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
}
