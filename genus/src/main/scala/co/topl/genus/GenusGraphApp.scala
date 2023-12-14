package co.topl.genus

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import cats.effect.{IO, IOApp}
import co.topl.catsakka._
import co.topl.genus.interpreters.orientdb.{OrientDb, OrientDbChainReplicator}
import co.topl.grpc.ToplGrpc
import org.typelevel.log4cats.slf4j.Slf4jLogger

object GenusGraphApp extends IOApp.Simple {
  type F[A] = IO[A]

  def run: IO[Unit] =
    AkkaCatsRuntime
      .systemResource[F, ActorSystem[Nothing]](ActorSystem(Behaviors.empty, "Genus"))
      .use(implicit system =>
        ToplGrpc.Client
          .make[F]("localhost", 8090, tls = false)
          .flatMap(implicit rpcClient =>
            Slf4jLogger
              .create[F]
              .flatMap(implicit logger =>
                OrientDb
                  .inMemory[F]
                  .use(factory =>
                    OrientDbChainReplicator
                      .make[F](factory.getNoTx)
                      .flatMap(_.replicateFrom(rpcClient))
                  )
              )
          )
      )
}
