package co.topl

import cats.arrow.FunctionK
import cats.~>

import scala.concurrent.Future

package object catsakka extends SourceOps with RunnableGraphOps with FlowOps {
  type FToFuture[F[_]] = F ~> Future
}
