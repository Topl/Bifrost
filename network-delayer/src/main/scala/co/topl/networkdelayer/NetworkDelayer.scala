package co.topl.networkdelayer

import cats.data.OptionT
import cats.effect.kernel.Resource
import cats.effect.{Async, IO}
import cats.implicits._
import co.topl.common.application.IOBaseApp
import com.comcast.ip4s._
import fs2._
import fs2.io.net.{Network, Socket}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object NetworkDelayer
    extends IOBaseApp[Args, ApplicationConfig](
      createArgs = args => Args.parserArgs.constructOrThrow(args),
      createConfig = IOBaseApp.createTypesafeConfig,
      parseConfig = (args, conf) => ApplicationConfig.unsafe(args, conf)
    ) {

  implicit private val logger: Logger[F] =
    Slf4jLogger.getLoggerFromClass[F](this.getClass)

  def run: IO[Unit] =
    for {
      _ <- Logger[F].info("Launching NetworkDelayer")
      _ <- Logger[F].info(show"args=$args")
      _ <- Logger[F].info(show"config=$appConfig")
      _ <- appConfig.routes.parTraverse(serverForRoute).void
    } yield ()

  /**
   * Handle the given route configuration by binding the local host/port and forwarding any incoming request payloads
   * to the route configuration's destination.  The configuration's throttle is also applied to the connection.
   */
  private def serverForRoute(route: ApplicationConfig.Route): F[Unit] =
    for {
      serverStream   <- buildServerStream(route)
      clientResource <- buildClientResource(route)
      _ <- Logger[F].info(s"Serving at binding=${route.bindHost}:${route.bindPort} with throttle=${route.throttle}")
      _ <- serverStream
        .mapAsync(1)(handleSocket(route)(clientResource))
        .compile
        .drain
    } yield ()

  /**
   * Validate the route settings for the local binding, and bind the port to return a stream of inbound connections
   */
  private def buildServerStream(route: ApplicationConfig.Route): F[Stream[F, Socket[F]]] =
    for {
      bindHost <- OptionT
        .fromOption[F](Host.fromString(route.bindHost))
        .getOrRaise(new IllegalArgumentException("Invalid bindHost"))
      bindPort <- OptionT
        .fromOption[F](Port.fromInt(route.bindPort))
        .getOrRaise(new IllegalArgumentException("Invalid bindPort"))
    } yield Network[F].server(bindHost.some, bindPort.some)

  /**
   * Validate the route's destination and construct a reusable destination connection resource.
   */
  private def buildClientResource(route: ApplicationConfig.Route): F[Resource[F, Socket[F]]] =
    for {
      destinationHost <- OptionT
        .fromOption[F](Host.fromString(route.destinationHost))
        .getOrRaise(new IllegalArgumentException("Invalid destinationHost"))
      destinationPort <- OptionT
        .fromOption[F](Port.fromInt(route.destinationPort))
        .getOrRaise(new IllegalArgumentException("Invalid destinationPort"))
    } yield Network[F].client(SocketAddress(destinationHost, destinationPort))

  /**
   * Forward inbound data from the local socket to the destination.  Forward inbound data from the destination
   * to the local socket.  Apply throttling if configured.
   */
  private def handleSocket(route: ApplicationConfig.Route)(clientResource: Resource[F, Socket[F]])(
    localSocket:                  Socket[F]
  ): F[Unit] =
    Logger[F].info(s"Accepted inbound connection at binding=${route.bindHost}:${route.bindPort}") >>
    clientResource
      .use(clientSocket =>
        Logger[F].info(
          s"Forwarding from binding=${route.bindHost}:${route.bindPort}" +
          s" to remote=${route.destinationHost}:${route.destinationPort}"
        ) >>
        (
          download(route)(clientSocket.reads)(localSocket.writes),
          upload(route)(localSocket.reads)(clientSocket.writes)
        ).parTupled.void
      )
      .handleErrorWith(Logger[F].error(_)("Connection failed"))

  /**
   * Handle the "download" side of the socket, and impose throttling
   */
  private def download(route: ApplicationConfig.Route)(in: Stream[F, Byte])(out: Pipe[F, Byte, Nothing]): F[Unit] =
    route.throttle
      .foldLeft(in)(_ through downloadThrottler(_))
      .through(out)
      .compile
      .drain

  /**
   * Handle the "upload" side of the socket, and impose throttling
   */
  private def upload(route: ApplicationConfig.Route)(in: Stream[F, Byte])(out: Pipe[F, Byte, Nothing]): F[Unit] =
    route.throttle
      .foldLeft(in)(_ through uploadThrottler(_))
      .through(out)
      .compile
      .drain

  private def downloadThrottler(throttle: ApplicationConfig.Route.Throttle): Pipe[F, Byte, Byte] =
    _.through(latencyThrottler(throttle.latency)).through(bandwidthThrottler(throttle.downloadBytesPerSecond))

  private def uploadThrottler(throttle: ApplicationConfig.Route.Throttle): Pipe[F, Byte, Byte] =
    _.through(latencyThrottler(throttle.latency)).through(bandwidthThrottler(throttle.uploadBytesPerSecond))

  private def bandwidthThrottler(bytesPerSecond: Long): Pipe[F, Byte, Byte] =
    _.metered(1.seconds / bytesPerSecond)

  private def latencyThrottler(latency: FiniteDuration): Pipe[F, Byte, Byte] =
    _.chunks.parEvalMapUnbounded(v => Async[F].delayBy(Async[F].pure(v), latency)).unchunks

}
