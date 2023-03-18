package co.topl.genusServer

import cats.effect.{Async, Resource}
import cats.implicits.catsSyntaxApplicativeId

object GenusRpcServer {

  def make[F[_]: Async](): Resource[F, GenusRpc[F]] =
    Resource.pure {
      new GenusRpc[F] {
        override def helloWorld(): F[String] = "helloworld".pure[F]
      }
    }

}
