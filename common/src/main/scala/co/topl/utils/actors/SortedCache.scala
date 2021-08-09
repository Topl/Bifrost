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
    case class Insert[T](values: Iterable[T]) extends ReceivableMessage[T]
    case class Pop[T](isViable: T => Boolean, replyTo: ActorRef[T]) extends ReceivableMessage[T]
  }

  def apply[T: Ordering: Show](
    itemLimit:    Int = Int.MaxValue,
    itemPopLimit: Int = Int.MaxValue
  ): Behavior[ReceivableMessage[T]] =
    Behaviors.setup { context =>
      stateful(
        Map.empty,
        Impl(Nil, itemLimit, itemPopLimit, onEvict = t => context.log.info("Evicting entry {}", t.show))
      )
    }

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
            stateful(stash - replyTo, nextCache)
          case None =>
            // Overwrite any existing Pop request for the given replyTo
            stateful(stash + (replyTo -> m), nextCache)
        }
    }

  implicit private def poppableItemOrdering[T](implicit ordering: Ordering[T]): Ordering[PoppableItem[T]] = (a, b) =>
    ordering.compare(a.item, b.item)

  private[actors] case class Impl[T: Ordering](
    items:        List[PoppableItem[T]],
    itemLimit:    Int,
    itemPopLimit: Int,
    onEvict:      T => Unit
  ) {

    def append(others: IterableOnce[T]): Impl[T] = {
      val (finalItems, sizeEvicted) =
        (items ++ others.iterator.map(PoppableItem(_, 0))).distinct.sorted.splitAt(itemLimit)
      sizeEvicted.map(_.item).foreach(onEvict)
      copy(finalItems)
    }

    def pop(isViable: T => Boolean): (Impl[T], Option[T]) =
      items.indexWhere(poppableBlock => isViable(poppableBlock.item)) match {
        case -1 =>
          val incremented =
            items.map(_.incremented)
          val (popLimitNonEvicted, popLimitEvicted) =
            incremented.partition(_.poppedCount < itemPopLimit)
          popLimitEvicted.map(_.item).foreach(onEvict)
          (copy(popLimitNonEvicted), None)
        case index =>
          val (popped, candidate :: unpopped) =
            items.splitAt(index)
          val incremented =
            popped.map(_.incremented)
          val (finalItems, popLimitEvicted) =
            (incremented ++ unpopped).partition(_.poppedCount < itemPopLimit)
          popLimitEvicted.map(_.item).foreach(onEvict)
          (
            copy(finalItems),
            Some(candidate.item)
          )
      }
  }

  private[actors] case class PoppableItem[T](item: T, poppedCount: Int) {
    def incremented: PoppableItem[T] = copy(poppedCount = poppedCount + 1)
  }

}
