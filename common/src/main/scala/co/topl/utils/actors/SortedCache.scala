package co.topl.utils.actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._
import cats.Show
import cats.implicits._

/**
 * A SortedCache is an actor that holds onto items in a sorted collection.  An item can be asynchronously popped off
 * when needed.
 */
object SortedCache {

  sealed abstract class ReceivableMessage[T]

  object ReceivableMessages {

    /**
     * Inserts the given items into the cache
     */
    case class Insert[T](values: Iterable[T]) extends ReceivableMessage[T]

    /**
     * Signals a request for data from the SortedCache.  If the SortedCache does not contain any candidates
     * that match the provided predicate, then the request is cached to be attempted again later once more
     * data is inserted.
     *
     * @param isViable A function which is applied to each item in the SortedCache.  If the function returns true,
     *                 the corresponding value is popped and returned.  If false, the request is saved for later.
     * @param replyTo The actor to receive the popped item.  If a request was already received for this given replyTo,
     *                the original is overwritten.  This behavior allows a requester to update the `isViable` function
     */
    case class Pop[T](isViable: T => Boolean, replyTo: ActorRef[T]) extends ReceivableMessage[T]
  }

  /**
   * Creates a SortedCache Behavior
   * @param itemLimit The maximum number of distinct items that can exist in the cache at a time.  If the cache
   *                  overflows, entries at the end of the Ordering will be evicted
   * @param itemPopLimit The maximum number of times that a single item can be attempted to be popped.  If an item
   *                     is determined not to be a viable candidate after this many attempts, it is evicted
   */
  def apply[T: Ordering: Show](
    itemLimit:    Int = Int.MaxValue,
    itemPopLimit: Int = Int.MaxValue
  ): Behavior[ReceivableMessage[T]] =
    Behaviors.setup { context =>
      stateful[T](
        Map.empty,
        Impl(IndexedSeq.empty, itemLimit, itemPopLimit, onEvict = t => context.log.info("Evicting entry {}", t.show))
      )
    }

  /**
   * A SortedCache behavior that captures its state within the function scope
   * @param stash A cache of pop requests that have been received for which we currently do not have any viable candidates
   * @param cache The implementation which holds onto the cached items in a sorted manner
   */
  private def stateful[T: Ordering](
    stash: Map[ActorRef[T], ReceivableMessages.Pop[T]],
    cache: Impl[T]
  ): Behaviors.Receive[ReceivableMessage[T]] =
    Behaviors.receive {
      case (context, ReceivableMessages.Insert(values)) =>
        stash.values.foreach(context.self.tell)
        stateful[T](Map.empty, cache.append(values))
      case (_, m @ ReceivableMessages.Pop(isViable, replyTo)) =>
        val (nextCache, maybeCandidate) =
          cache.pop(isViable)
        maybeCandidate match {
          case Some(candidate) =>
            replyTo ! candidate
            stateful[T](stash - replyTo, nextCache)
          case None =>
            // Overwrite any existing Pop request for the given replyTo
            stateful[T](stash + (replyTo -> m), nextCache)
        }
    }

  /**
   * Wraps some item `T` with a counter indicating the number of attempts to pop this item
   */
  private[actors] case class PoppableItem[T](item: T, poppedCount: Int) {
    def incremented: PoppableItem[T] = copy(poppedCount = poppedCount + 1)
  }

  /**
   * Provides ordering for `PoppableItem[T]` from some base `Ordering[T]`
   */
  implicit private def poppableItemOrdering[T](implicit ordering: Ordering[T]): Ordering[PoppableItem[T]] = (a, b) =>
    ordering.compare(a.item, b.item)

  /**
   * A SortedCache implementation which holds some item T in a sorted collection.  The implementation tracks the
   * number of attempts for each item in the cache and is responsible for evicting entries as necessary
   * @param onEvict A function which is called any time an entry is evicted
   */
  private[actors] case class Impl[T: Ordering](
    items:        IndexedSeq[PoppableItem[T]],
    itemLimit:    Int,
    itemPopLimit: Int,
    onEvict:      T => Unit
  ) {

    /**
     * Inserts the given items T into the items cache.  If the cache overflows, items are evicted and the `onEvict`
     * function is called.
     */
    def append(others: IterableOnce[T]): Impl[T] = {
      // For future optimization: `.distinct.sorted` is expensive.  Is it cheaper to use a SortedSet instead?
      val (finalItems, sizeEvicted) =
        (items ++ others.iterator.map(PoppableItem(_, 0))).distinct.sorted.splitAt(itemLimit)
      sizeEvicted.map(_.item).foreach(onEvict)
      copy(items = finalItems)
    }

    /**
     * Attempt to pop the next item from the cache using the given predicate.  As items are attempted, their
     * counts are incremented.  If an item is a candidate, it is returned along with an updated Impl state containing
     * updated counts for the items that were attempted.  If no candidates are found, an updated Impl state containing
     * updated counts for all items is returned.  Any items which exceed the `itemPopLimit` are evicted and the `onEvict`
     * function is invoked
     * @param isViable A predicate to be applied to each item until one succeeds
     */
    def pop(isViable: T => Boolean): (Impl[T], Option[T]) =
      items.indexWhere(poppableBlock => isViable(poppableBlock.item)) match {
        case -1 =>
          val incremented =
            items.map(_.incremented)
          val (popLimitNonEvicted, popLimitEvicted) =
            incremented.partition(_.poppedCount < itemPopLimit)
          popLimitEvicted.map(_.item).foreach(onEvict)
          (copy(items = popLimitNonEvicted), None)
        case index =>
          val (popped, candidate +: unpopped) =
            items.splitAt(index)
          // Items _before_ the candidate did not pass the `isViable` predicate, thus they were "attempted"
          // and need to be incremented.  Items _after_ the candidate were never attempted, so their
          // counts can remain the same
          val incremented =
            popped.map(_.incremented)
          val (finalItems, popLimitEvicted) =
            (incremented ++ unpopped).partition(_.poppedCount < itemPopLimit)
          popLimitEvicted.map(_.item).foreach(onEvict)
          (copy(items = finalItems), Some(candidate.item))
      }
  }

}
