package co.topl.tetra.it.util

import cats.effect._
import cats.implicits._
import cats.effect.implicits._
import co.topl.algebras.ToplRpc
import co.topl.grpc.ToplGrpc
import com.spotify.docker.client.DockerClient
import org.typelevel.log4cats.Logger
import fs2._
import fs2.io.file.Files
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.charset.StandardCharsets

class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  implicit def logger[F[_]: Sync]: Logger[F] =
    Slf4jLogger.getLoggerFromName[F](s"DockerNode(${containerId.take(8)})")

  def startContainer[F[_]: Async]: F[Unit] =
    for {
      _  <- Logger[F].info("Starting")
      _  <- Sync[F].blocking(dockerClient.startContainer(containerId))
      _  <- awaitContainerStart
      ip <- ipAddress
      _  <- Logger[F].info(s"Successfully started container $containerId on IP $ip")
    } yield ()

  def restartContainer[F[_] : Async]: F[Unit] =
    for {
      _ <- Logger[F].info("Restarting")
      _ <- Sync[F].blocking(dockerClient.restartContainer(containerId))
      _ <- awaitContainerStart
      ip <- ipAddress
      _ <- Logger[F].info(s"Successfully restarted container $containerId on IP $ip")
    } yield ()

  def stop[F[_]: Async]: F[Unit] =
    Logger[F].info("Stopping") >>
    Sync[F].blocking(dockerClient.stopContainer(containerId, 10))

  def ipAddress[F[_]: Sync]: F[String] =
    Sync[F].blocking(dockerClient.inspectContainer(containerId).networkSettings().ipAddress())

  def rpcClient[F[_]: Async]: Resource[F, ToplRpc[F, Stream[F, *]]] =
    ipAddress.toResource.flatMap(ToplGrpc.Client.make[F](_, 9084, tls = false)) // TODO: Don't hardcode

  def configure[F[_]: Async](configYaml: String): F[Unit] =
    for {
      _         <- Logger[F].info("Reconfiguring container")
      isRunning <- Sync[F].blocking(dockerClient.inspectContainer(containerId).state().running())
      _ <-
        if (isRunning)
          Sync[F].raiseError(new IllegalStateException("Attempted to set configuration on running node"))
        else Sync[F].unit
      _ <- Files[F].tempDirectory.use(tmpConfigDir =>
        for {
          tmpConfigFile <- (tmpConfigDir / "node.yaml").pure[F]
          _ <- Stream
            .chunk(Chunk.array(configYaml.getBytes(StandardCharsets.UTF_8)))
            .through(Files[F].writeAll(tmpConfigFile))
            .compile
            .drain
          _ <- Sync[F].blocking(
            dockerClient.copyToContainer(
              tmpConfigDir.toNioPath,
              containerId,
              "/opt/docker/config"
            )
          )
        } yield ()
      )
    } yield ()

  private def awaitContainerStart[F[_]: Async]: F[Unit] = {
    def go(remainingAttempts: Int): F[Unit] =
      if (remainingAttempts > 0) ipAddress.void.recoverWith { case _ =>
        go(remainingAttempts - 1)
      }
      else Async[F].raiseError(throw new IllegalStateException(s"Container IP not found. ContainerId=$containerId"))

    go(10)
  }

}
