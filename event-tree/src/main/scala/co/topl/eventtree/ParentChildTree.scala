package co.topl.eventtree

import cats._
import cats.data._
import cats.effect._
import cats.effect.std.Semaphore
import cats.implicits._

trait ParentChildTree[F[_], T] {

  /**
   * Returns the parent `T` of the given child `T`.  If the child has no parent, it is either an orphan or it is the root
   * of all other children.
   * @param t a child T
   */
  def parentOf(t: T): F[Option[T]]

  /**
   * Create a connection between a child and a parent
   */
  def associate(child: T, parent: T): F[Unit]

  /**
   * Determines the number of ancestors from its nearest orphan/root n-parent.
   * @param t the child
   */
  def heightOf(t: T): F[Long]

  /**
   * Traces back from each of the given children until a common ancestor is found.  The traversed items are returned.
   * @param a a child T
   * @param b another child T
   * @return a tuple containing (inclusive traversal from `a` to common ancestor, inclusive traversal from `b` to common ancestor)
   */
  def findCommonAncestor(a: T, b: T): F[(NonEmptyChain[T], NonEmptyChain[T])]
}

object ParentChildTree {

  object FromRef {

    def make[F[_]: Sync, T: Eq: Show]: F[ParentChildTree[F, T]] =
      Ref
        .of[F, Map[T, T]](Map.empty[T, T])
        .map(new Impl[F, T](_))

    private class Impl[F[_]: Sync, T: Eq: Show](ref: Ref[F, Map[T, T]]) extends ParentChildTree[F, T] {

      def parentOf(t: T): F[Option[T]] =
        ref.get.map(_.get(t))

      def associate(child: T, parent: T): F[Unit] =
        ref.update(_.updated(child, parent))

      // TODO: Caching
      def heightOf(t: T): F[Long] =
        (t, 0L)
          .tailRecM { case (t, distance) =>
            OptionT(parentOf(t))
              .fold[Either[(T, Long), (T, Long)]]((t, distance).asRight)(p => (p, distance + 1).asLeft)
          }
          .map(_._2)

      private def prependWithParent(c: NonEmptyChain[T]): F[NonEmptyChain[T]] =
        OptionT(parentOf(c.head))
          .foldF[NonEmptyChain[T]](MonadThrow[F].raiseError(new NoSuchElementException(c.head.show)))(
            c.prepend(_).pure[F]
          )

      def findCommonAncestor(a: T, b: T): F[(NonEmptyChain[T], NonEmptyChain[T])] =
        Monad[F].ifElseM(
          Sync[F].delay(a === b) -> Sync[F].delay((NonEmptyChain(a), NonEmptyChain(b)))
        )(
          Sync[F].defer(
            for {
              (aHeight, bHeight) <- (heightOf(a), heightOf(b)).tupled
              (aAtEqualHeight, bAtEqualHeight) <- Monad[F]
                .ifElseM(
                  Sync[F].delay(aHeight === bHeight) -> Sync[F].delay((NonEmptyChain(a), NonEmptyChain(b))),
                  Sync[F].delay(aHeight < bHeight) -> Sync[F].defer(
                    traverseBackToHeight(NonEmptyChain(b), bHeight, aHeight)
                      .map(NonEmptyChain(a) -> _._1)
                  )
                )(
                  Sync[F].defer(
                    traverseBackToHeight(NonEmptyChain(a), aHeight, bHeight)
                      .map(_._1 -> NonEmptyChain(b))
                  )
                )
              (chainA, chainB) <- (aAtEqualHeight, bAtEqualHeight).iterateUntilM { case (aChain, bChain) =>
                (prependWithParent(aChain), prependWithParent(bChain)).tupled
              } { case (aChain, bChain) => aChain.head === bChain.head }
            } yield (chainA, chainB)
          )
        )

      private def traverseBackToHeight(
        collection:    NonEmptyChain[T],
        initialHeight: Long,
        targetHeight:  Long
      ): F[(NonEmptyChain[T], Long)] =
        Sync[F].defer(
          (collection, initialHeight)
            .iterateUntilM { case (chain, height) =>
              prependWithParent(chain).map(_ -> (height - 1))
            }(_._2 === targetHeight)
        )
    }
  }

  object FromSemaphore {

    def make[F[_]: Async, T: Eq: Show]: F[ParentChildTree[F, T]] =
      Semaphore[F](1)
        .map(new Impl[F, T](_))

    private class Impl[F[_]: Sync, T: Eq: Show](semaphore: Semaphore[F]) extends ParentChildTree[F, T] {

      private var data = Map.empty[T, T]

      def parentOf(t: T): F[Option[T]] =
        semaphore.permit.use(_ => parentOfUnsafe(t).pure[F])

      private def parentOfUnsafe(t: T) = data.get(t)

      def associate(child: T, parent: T): F[Unit] =
        semaphore.permit.use(_ => (data = data.updated(child, parent)).pure[F])

      // TODO: Caching
      def heightOf(t: T): F[Long] =
        semaphore.permit.use(_ => heightOfUnsafe(t))

      private def heightOfUnsafe(t: T) =
        (t, 0L)
          .tailRecM { case (t, distance) =>
            OptionT
              .fromOption[F](parentOfUnsafe(t))
              .fold[Either[(T, Long), (T, Long)]]((t, distance).asRight)(p => (p, distance + 1).asLeft)
          }
          .map(_._2)

      private def prependWithParent(c: NonEmptyChain[T]): F[NonEmptyChain[T]] =
        OptionT
          .fromOption[F](parentOfUnsafe(c.head))
          .foldF[NonEmptyChain[T]](MonadThrow[F].raiseError(new NoSuchElementException(c.head.show)))(
            c.prepend(_).pure[F]
          )

      def findCommonAncestor(a: T, b: T): F[(NonEmptyChain[T], NonEmptyChain[T])] =
        semaphore.permit.use(_ =>
          Sync[F]
            .delay(a === b)
            .ifM(
              Sync[F].delay((NonEmptyChain(a), NonEmptyChain(b))),
              Sync[F].defer(
                for {
                  (aHeight, bHeight) <- (heightOfUnsafe(a), heightOfUnsafe(b)).tupled
                  (aAtEqualHeight, bAtEqualHeight) <- Monad[F]
                    .ifElseM(
                      Sync[F].delay(aHeight === bHeight) -> Sync[F].delay((NonEmptyChain(a), NonEmptyChain(b))),
                      Sync[F].delay(aHeight < bHeight) -> Sync[F].defer(
                        traverseBackToHeight(NonEmptyChain(b), bHeight, aHeight)
                          .map(NonEmptyChain(a) -> _._1)
                      )
                    )(
                      Sync[F].defer(
                        traverseBackToHeight(NonEmptyChain(a), aHeight, bHeight)
                          .map(_._1 -> NonEmptyChain(b))
                      )
                    )
                  (chainA, chainB) <- (aAtEqualHeight, bAtEqualHeight).iterateUntilM { case (aChain, bChain) =>
                    (prependWithParent(aChain), prependWithParent(bChain)).tupled
                  } { case (aChain, bChain) => aChain.head === bChain.head }
                } yield (chainA, chainB)
              )
            )
        )

      private def traverseBackToHeight(
        collection:    NonEmptyChain[T],
        initialHeight: Long,
        targetHeight:  Long
      ): F[(NonEmptyChain[T], Long)] =
        Sync[F].defer(
          (collection, initialHeight)
            .iterateUntilM { case (chain, height) =>
              prependWithParent(chain).map(_ -> (height - 1))
            }(_._2 === targetHeight)
        )
    }
  }
}
