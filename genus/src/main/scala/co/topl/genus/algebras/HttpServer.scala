package co.topl.genus.algebras

import akka.http.scaladsl.Http

trait HttpServer[F[_]] {
  def run: F[Http.ServerBinding]
}
