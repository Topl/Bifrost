package co.topl.byzantine.util

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import co.topl.algebras.{NodeRpc, ToplGenusRpc}
import co.topl.genus.GenusGrpc
import co.topl.grpc.NodeGrpc
import com.spotify.docker.client.DockerClient
import com.spotify.docker.client.messages.HostConfig
import fs2._
import fs2.io.file.{Files, Flags, Path}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters._

class NodeDockerApi(containerId: String)(implicit dockerClient: DockerClient) {

  implicit def logger[F[_]: Sync]: Logger[F] =
    Slf4jLogger.getLoggerFromName[F](s"DockerNode(${containerId.take(8)})")

  def startContainer[F[_]: Async]: F[Unit] =
    for {
      _  <- Logger[F].info("Starting")
      _  <- Sync[F].blocking(dockerClient.startContainer(containerId))
      _  <- awaitContainerStart
      ip <- ipAddress
      _  <- Logger[F].info(s"Successfully started container on IP $ip")
    } yield ()

  def restartContainer[F[_]: Async]: F[Unit] =
    for {
      _  <- Logger[F].info("Restarting")
      _  <- Sync[F].blocking(dockerClient.restartContainer(containerId))
      _  <- awaitContainerStart
      ip <- ipAddress
      _  <- Logger[F].info(s"Successfully restarted container on IP $ip")
    } yield ()

  def stop[F[_]: Async]: F[Unit] =
    Logger[F].info("Stopping") >>
    Sync[F].blocking(dockerClient.stopContainer(containerId, 10))

  def ipAddress[F[_]: Sync]: F[String] =
    Sync[F].blocking(dockerClient.inspectContainer(containerId).networkSettings().ipAddress())

  def rpcClient[F[_]: Async](port: Int, tls: Boolean = false): Resource[F, NodeRpc[F, Stream[F, *]]] =
    ipAddress.toResource.flatMap(NodeGrpc.Client.make[F](_, port, tls))

  def rpcGenusClient[F[_]: Async](port: Int, tls: Boolean = false): Resource[F, ToplGenusRpc[F]] =
    ipAddress.toResource.flatMap(GenusGrpc.Client.make[F](_, port, tls))

  def configure[F[_]: Async](configYaml: String): F[Unit] =
    for {
      _         <- Logger[F].info("Reconfiguring container")
      isRunning <- Sync[F].blocking(dockerClient.inspectContainer(containerId).state().running())
      _ <-
        if (isRunning)
          Sync[F].raiseError(new IllegalStateException("Attempted to set configuration on running node"))
        else Sync[F].unit
      _ <- Files
        .forAsync[F]
        .tempDirectory
        .use(tmpConfigDir =>
          for {
            tmpConfigFile <- (tmpConfigDir / "node.yaml").pure[F]
            tmpLogFile    <- (tmpConfigDir / "logback.xml").pure[F]
            _ <- Stream
              .chunk(Chunk.array(configYaml.getBytes(StandardCharsets.UTF_8)))
              .through(Files.forAsync[F].writeAll(tmpConfigFile))
              .compile
              .drain
            _ <- fs2.io
              .readClassLoaderResource("logback-container.xml")
              .through(Files.forAsync[F].writeAll(tmpLogFile))
              .compile
              .drain
            _ <- copyDirectoryIntoContainer(tmpConfigDir, Path("/bifrost-config"))
          } yield ()
        )
    } yield ()

  def copyDirectoryIntoContainer[F[_]: Sync](localPath: Path, containerPath: Path): F[Unit] =
    Sync[F].blocking(
      dockerClient.copyToContainer(
        localPath.toNioPath,
        containerId,
        containerPath.toString
      )
    )

  def bindDirectoryIntoContainer[F[_]: Sync](localPath: Path, containerPath: Path): F[Unit] =
    Sync[F].blocking(
      dockerClient.updateContainer(
        containerId,
        dockerClient
          .inspectContainer(containerId)
          .hostConfig()
          .toBuilder
          .appendBinds(
            HostConfig.Bind
              .builder()
              .from(localPath.toString)
              .to(containerPath.toString)
              .selinuxLabeling(true)
              .build()
          )
          .build()
      )
    )

  def containerLogs[F[_]: Async]: Stream[F, Byte] =
    Stream
      .fromAutoCloseable(
        Sync[F].blocking(
          dockerClient.logs(containerId, DockerClient.LogsParam.stdout(), DockerClient.LogsParam.stderr())
        )
      )
      .map(_.asScala)
      .flatMap(Stream.fromBlockingIterator[F](_, 1))
      .map(_.content())
      .map(Chunk.byteBuffer)
      .unchunks

  def saveContainerLogs[F[_]: Async](file: Path): F[Unit] =
    Sync[F].defer(
      Logger[F].info(s"Writing container logs to $file") >>
      containerLogs[F]
        .through(Files.forAsync[F].writeAll(file, Flags.Write))
        .compile
        .drain
    )

  private def awaitContainerStart[F[_]: Async]: F[Unit] = {
    def go(remainingAttempts: Int): F[Unit] =
      if (remainingAttempts > 0) ipAddress.void.recoverWith { case _ =>
        go(remainingAttempts - 1)
      }
      else Async[F].raiseError(throw new IllegalStateException(s"Container IP not found. ContainerId=$containerId"))

    go(10)
  }

}
