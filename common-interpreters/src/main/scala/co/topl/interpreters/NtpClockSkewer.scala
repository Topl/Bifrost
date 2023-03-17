package co.topl.interpreters

import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import fs2._
import org.apache.commons.net.ntp.NTPUDPClient
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.net.InetAddress
import scala.concurrent.duration._

object NtpClockSkewer {

  /**
   * Constructs a function that returns the current difference between the local clock and the NTP server.
   * @param ntpServer The server to call for NTP offset information
   * @param refreshInterval The interval at which to refresh the offset
   * @param timeout How long to wait for an NTP response
   */
  def make[F[_]: Async](
    ntpServer:       String,
    refreshInterval: FiniteDuration,
    timeout:         FiniteDuration
  ): Resource[F, () => F[Long]] =
    for {
      ntpClient <- Resource.make(Sync[F].blocking(new NTPUDPClient()))(client => Sync[F].blocking(client.close()))
      _ = ntpClient.setDefaultTimeout(timeout.toMillis.toInt)
      _ <- Sync[F].blocking(ntpClient.open()).toResource
      implicit0(logger: Logger[F]) = Slf4jLogger.getLoggerFromName("Bifrost.NTP")
      skewRef <- Ref.of[F, Long](0L).toResource
      _ <- Stream
        .fixedRateStartImmediately(refreshInterval)
        .flatMap(_ =>
          Stream.retry(
            Sync[F]
              .blocking(ntpClient.getTime(InetAddress.getByName(ntpServer)))
              .timeout(timeout),
            timeout,
            identity,
            maxAttempts = 10
          )
        )
        .map { info =>
          info.computeDetails()
          info.getOffset: Long
        }
        .evalTap(offset => Logger[F].info(show"Setting NTP offset=${offset}ms"))
        .evalTap(skewRef.set)
        .compile
        .drain
        .background
    } yield () => skewRef.get

}
