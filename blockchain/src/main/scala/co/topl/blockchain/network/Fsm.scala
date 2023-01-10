package co.topl.blockchain.network

import cats.implicits
import cats.syntax._
import cats.{Applicative, Functor, Id}
import cats.Functor
import cats.implicits._
import cats._

/** F[_] - Effect S - State I - Input O - Output
 */
case class Fsm[F[_], S, I, O](run: (S, I) => F[(S, O)]) {
  def runS(implicit functor: Functor[F]): (S, I) => F[S] = (s, i) => run(s, i).map(_._1)
}

object Fsm {
  def id[S, I, O](run: (S, I) => Id[(S, O)]): Fsm[Id, S, I, O] = Fsm(run)

  def pure[F[_] : Applicative, S, I, O](run: (S, I) => (S, O)): Fsm[F, S, I, O] =
    Fsm { case (s, in) => Applicative[F].pure(run(s, in)) }
}