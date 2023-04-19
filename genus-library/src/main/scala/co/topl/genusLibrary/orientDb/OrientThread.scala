package co.topl.genusLibrary.orientDb

import cats.effect.Async
import cats.effect.Resource

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/**
 * Provides an interface for executing OrientDB operations on a dedicated thread
 */
trait OrientThread[F[_]] {

  /**
   * Execute the given thunk on the dedicated thread
   */
  def exec[O](t: => O): F[O]

  /**
   * Execute the given F-operation on the dedicated thread
   */
  def execF[O](t: => F[O]): F[O]

}

object OrientThread {

  def apply[F[_]: OrientThread]: OrientThread[F] = implicitly[OrientThread[F]]

  def create[F[_]: Async]: Resource[F, OrientThread[F]] = for {
    executor <- Resource
      .make(Async[F].delay(Executors.newSingleThreadExecutor()))(ec => Async[F].delay(ec.shutdown()))
    ec = ExecutionContext.fromExecutor(executor)
    orientThread = new OrientThread[F] {
      override def exec[O](t: => O): F[O] = execF(Async[F].delay(t))

      def execF[O](t: => F[O]): F[O] = Async[F].evalOn(Async[F].defer(t), ec)
    }
  } yield orientThread

}
