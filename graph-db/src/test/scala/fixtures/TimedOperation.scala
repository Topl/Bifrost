package fixtures

import scala.concurrent.duration._

object TimedOperation {

  def timed[T](name: String, operation: => T)(implicit completionHandler: TimedOperationCompletionHandler): T = {
    val start = System.nanoTime()
    val r = operation
    val duration = (System.nanoTime() - start).nanos
    completionHandler.onComplete(name, duration)
    r
  }
}

trait TimedOperationCompletionHandler {
  def onComplete(name: String, duration: Duration): Unit
}
