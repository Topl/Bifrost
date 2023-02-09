package co.topl.catsakka

import cats.effect.{Async, Resource}
import cats.implicits._
import fs2.concurrent.Topic

object DroppingTopic {

  /**
   * Rebroadcasts the given Topic into a new Topic.  If subscribers of the new topic are unable to keep up,
   * messages are discarded rather than slowing down the original Topic.
   * @param topic a base Topic that produces data and should not be slowed down
   * @param buffer the number of elements to enqueue locally
   */
  def apply[F[_]: Async, T](topic: Topic[F, T], buffer: Int): Resource[F, Topic[F, T]] =
    for {
      subTopic <- Resource.make(Topic[F, T])(_.close.void)
      stream   <- topic.subscribeAwaitUnbounded
      _        <- Async[F].background(stream.dropOldest(buffer).through(subTopic.publish).compile.drain)
    } yield subTopic
}
