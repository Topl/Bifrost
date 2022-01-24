package co.topl.genus.algebras

import akka.http.scaladsl.Http

trait HttpServerAlg[F[_]] {
  def run: F[Http.ServerBinding]
}
