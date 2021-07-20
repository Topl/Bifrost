package co.topl.utils.actors

import akka.actor.typed._
import akka.actor.typed.scaladsl._

/**
 * A SortedCache is an actor that holds onto items in a sorted collection.  An item can be asynchronously popped off
 * when needed.
 *
 * TODO: Timeout
 */
object SortedCache {

  sealed abstract class ReceivableMessage[T]

  object ReceivableMessages {
    case class Insert[T](values: Iterable[T]) extends ReceivableMessage[T]
    case class Pop[T](isViable: T => Boolean, replyTo: ActorRef[T]) extends ReceivableMessage[T]
  }

  final val DefaultItemPopLimit = 50

  def apply[T: Ordering](itemPopLimit: Int = DefaultItemPopLimit): Behavior[ReceivableMessage[T]] =
    stateful(Map.empty, Impl(Nil, itemPopLimit))

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

  private[actors] case class Impl[T: Ordering](items: List[PoppableItem[T]], itemPopLimit: Int) {

    def append(others: IterableOnce[T]): Impl[T] =
      copy(
        (items ++ others.iterator.map(PoppableItem(_, 0))).sorted
      )

    def pop(isViable: T => Boolean): (Impl[T], Option[T]) =
      items.indexWhere(poppableBlock => isViable(poppableBlock.item)) match {
        case -1 =>
          // TODO: Logging when an entry is evicted
          (copy(items.map(_.incremented).filterNot(_.poppedCount >= itemPopLimit)), None)
        case index =>
          val (popped, candidate, unpopped) = {
            val (a, b) = items.splitAt(index)
            (a, b.head, b.tail)
          }
          (
            // TODO: Logging when an entry is evicted
            copy(popped.map(_.incremented).filterNot(_.poppedCount >= itemPopLimit) ++ unpopped),
            Some(candidate.item)
          )
      }
  }

  private[actors] case class PoppableItem[T](item: T, poppedCount: Int) {
    def incremented: PoppableItem[T] = copy(poppedCount = poppedCount + 1)
  }

}
