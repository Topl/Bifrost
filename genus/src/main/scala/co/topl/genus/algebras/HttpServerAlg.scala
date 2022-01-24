package co.topl.genus.algebras

import akka.http.scaladsl.Http

/**
 * Represents an http server which can be bound to a particular address and port.
 * @tparam F the effect-ful type of the result from the server binding
 */
trait HttpServerAlg[F[_]] {
  def run: F[Http.ServerBinding]
}
