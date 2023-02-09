package co.topl.eventtree

import cats._
import cats.data.{NonEmptyChain, OptionT}
import cats.effect._
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

  /**
   * A ParentChildTree backed by a Store.  The Store maps a block ID to a tuple containing (its height, its parent ID).
   */
  object FromReadWrite {

    def make[F[_]: Async, T: Eq: Show](
      read:  T => F[Option[(Long, T)]],
      write: (T, (Long, T)) => F[Unit],
      root:  T
    ): F[ParentChildTree[F, T]] =
      Async[F].delay(new Impl(read, write, root))

    private class Impl[F[_]: Async, T: Eq: Show](
      read:  T => F[Option[(Long, T)]],
      write: (T, (Long, T)) => F[Unit],
      root:  T
    ) extends ParentChildTree[F, T] {

      private def readOrRaise(id: T) =
        OptionT(read(id)).getOrElseF(Async[F].raiseError(new NoSuchElementException(show"Element not found. id=$id")))

      def parentOf(t: T): F[Option[T]] =
        if (t === root) none[T].pure[F]
        else OptionT(read(t)).map(_._2).value

      def associate(child: T, parent: T): F[Unit] =
        if (parent === root) write(child, (1, parent))
        else readOrRaise(parent).flatMap { case (height, _) => write(child, (height + 1, parent)) }

      def heightOf(t: T): F[Long] =
        if (t === root) 0L.pure[F]
        else readOrRaise(t).map(_._1)

      def findCommonAncestor(a: T, b: T): F[(NonEmptyChain[T], NonEmptyChain[T])] =
        if (a === b) (NonEmptyChain(a), NonEmptyChain(b)).pure[F]
        else
          for {
            (aHeight, bHeight) <- (heightOf(a), heightOf(b)).tupled
            (aAtEqualHeight, bAtEqualHeight) <-
              if (aHeight === bHeight) (NonEmptyChain(a), NonEmptyChain(b)).pure[F]
              else if (aHeight < bHeight)
                traverseBackToHeight(NonEmptyChain(b), bHeight, aHeight).map(NonEmptyChain(a) -> _._1)
              else traverseBackToHeight(NonEmptyChain(a), aHeight, bHeight).map(_._1 -> NonEmptyChain(b))
            (chainA, chainB) <- (aAtEqualHeight, bAtEqualHeight).iterateUntilM { case (aChain, bChain) =>
              (prependWithParent(aChain), prependWithParent(bChain)).tupled
            } { case (aChain, bChain) => aChain.head === bChain.head }
          } yield (chainA, chainB)

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

      private def prependWithParent(c: NonEmptyChain[T]): F[NonEmptyChain[T]] =
        readOrRaise(c.head).map(_._2).map(c.prepend)
    }
  }
}
