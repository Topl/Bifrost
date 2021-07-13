package co.topl.utils.akka

import akka.actor.typed._
import akka.actor.typed.scaladsl._

object SortedCache {

  sealed abstract class ReceivableMessage[T]

  object ReceivableMessages {
    case class Insert[T](values: Iterable[T]) extends ReceivableMessage[T]
    case class Pop[T](isViable: T => Boolean, replyTo: ActorRef[T]) extends ReceivableMessage[T]
  }

  private val StashSize = 100

  def apply[T: Ordering](itemPopLimit: Int = 50): Behavior[ReceivableMessage[T]] =
    Behaviors.withStash(StashSize)(stateful(_, Impl(Nil, itemPopLimit)))

  private def stateful[T: Ordering](
    stash: StashBuffer[ReceivableMessage[T]],
    cache: Impl[T]
  ): Behaviors.Receive[ReceivableMessage[T]] =
    Behaviors.receiveMessage {
      case ReceivableMessages.Insert(values) =>
        stash.unstashAll(stateful(stash, cache.append(values)))
      case m @ ReceivableMessages.Pop(isViable, replyTo) =>
        val (nextCache, maybeCandidate) =
          cache.pop(isViable)
        maybeCandidate match {
          case Some(candidate) =>
            replyTo ! candidate
          case None =>
            stash.stash(m)
        }
        stateful(stash, nextCache)
    }

  implicit private def poppableItemOrdering[T](implicit ordering: Ordering[T]): Ordering[PoppableItem[T]] = (a, b) =>
    ordering.compare(a.item, b.item)

  private[akka] case class Impl[T: Ordering](items: List[PoppableItem[T]], itemPopLimit: Int) {

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

  private[akka] case class PoppableItem[T](item: T, poppedCount: Int) {
    def incremented: PoppableItem[T] = copy(poppedCount = poppedCount + 1)
  }

}
