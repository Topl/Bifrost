package co.topl.genusLibrary.orientDb

import cats.effect.Async
import cats.effect.Resource

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

/**
 * Provides an interface for executing OrientDB operations on a dedicated thread.  OrientDB is "extra thread-safe", meaning
 * it requires the database to be registered on the current thread for several types of interactions.  This OrientThread
 * helper assists with running computations on a thread that is properly registered.
 */
trait OrientThread[F[_]] {

  /**
   * Execute the given thunk on the dedicated thread
   */
  def delay[O](t: => O): F[O]

  /**
   * Execute the given F-operation on the dedicated thread
   */
  def defer[O](t: => F[O]): F[O]

}

object OrientThread {

  def apply[F[_]: OrientThread]: OrientThread[F] = implicitly[OrientThread[F]]

  def create[F[_]: Async]: Resource[F, OrientThread[F]] = for {
    executor <- Resource
      .make(Async[F].delay(Executors.newSingleThreadExecutor()))(ec => Async[F].delay(ec.shutdown()))
    ec = ExecutionContext.fromExecutor(executor)
    orientThread = new OrientThread[F] {
      override def delay[O](t: => O): F[O] = defer(Async[F].delay(t))

      def defer[O](t: => F[O]): F[O] = Async[F].evalOn(Async[F].defer(t), ec)
    }
  } yield orientThread

}
