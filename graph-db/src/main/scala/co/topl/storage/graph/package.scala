package co.topl.storage

import akka.NotUsed
import akka.stream.scaladsl.Source

import scala.concurrent.{ExecutionContext, Future}

package object graph {

  private[graph] def iteratorSourceOnDispatcher[T](
    iteratorF:                () => Iterator[T],
    blockingExecutionContext: ExecutionContext
  ): Source[T, NotUsed] =
    Source
      .lazySource { () =>
        val it = iteratorF()
        Source
          .repeat {}
          .takeWhile(_ => it.hasNext)
          .mapAsync(1)(_ => Future(it.next())(blockingExecutionContext))
      }
      .mapMaterializedValue(_ => NotUsed)
}
