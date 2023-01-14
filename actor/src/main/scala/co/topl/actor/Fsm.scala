package co.topl.actor

import cats.implicits._
import cats.{Applicative, Functor, Id}

/**
 * Internal state for actor
 * @param run function to reach next state
 * @tparam F effect
 * @tparam S state
 * @tparam I input message type
 * @tparam O output message type
 */
case class Fsm[F[_], S, I, O](run: (S, I) => F[(S, O)])

object Fsm {
  def pure[F[_] : Applicative, S, I, O](run: (S, I) => (S, O)): Fsm[F, S, I, O] =
    Fsm { case (s, in) => Applicative[F].pure(run(s, in)) }
}