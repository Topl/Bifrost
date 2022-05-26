package co.topl.simulator.eligibility

import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import cats.arrow.FunctionK
import cats.effect.kernel.Sync
import cats.effect.{Async, IO, IOApp}
import cats.implicits._
import cats.~>
import co.topl.catsakka._
import co.topl.consensus.LeaderElectionValidation
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.crypto.hash.Blake2b512
import co.topl.crypto.mnemonic.Entropy
import co.topl.crypto.signing.Ed25519VRF
import co.topl.interpreters.{ActorPoolUnsafeResource, SchedulerClock, StatsInterpreter}
import co.topl.models.utility.Ratio
import co.topl.numerics.{ExpInterpreter, Log1pInterpreter}
import co.topl.typeclasses.implicits.Ops
import io.circe.Json
import io.circe.syntax._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.nio.file.{Files, Paths}
import java.time.Instant
import java.util.UUID
import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

object ThresholdSimulator extends IOApp.Simple {

  private val testConfigs =
    for {
      precision     <- List(16, 32, 64)
      amplitude     <- List.tabulate(20)(t => Ratio(t * 5, 100))
      relativeStake <- List.tabulate(10)(t => Ratio(t * 10, 100))
    } yield (precision, amplitude, relativeStake)

  private val OperationalPeriodLength = 180L
  private val OperationalPeriodsPerEpoch = 4L
  private val EpochLength = OperationalPeriodLength * OperationalPeriodsPerEpoch
  private val SlotDuration = 10.milli
  private val TestName = "ThresholdGenerator"

  type F[A] = IO[A]

  implicit private val logger: Logger[F] = Slf4jLogger.getLogger[F]
  private val statsDir = Paths.get(".bifrost", "stats")
  Files.createDirectories(statsDir)

  implicit private val system: ActorSystem[_] =
    ActorSystem(
      Behaviors.empty,
      "ThresholdSimulator"
    )

  implicit val fToIo: ~>[F, IO] = FunctionK.id

  implicit private val timeout: Timeout = Timeout(20.seconds)

  override def run: IO[Unit] = {
    for {
      ed25519VRFResource <- ActorPoolUnsafeResource.Eval.make[F, Ed25519VRF](Ed25519VRF.precomputed(), _ => ())
      blake2b512Resource <- ActorPoolUnsafeResource.Eval.make[F, Blake2b512](new Blake2b512, _ => ())
      (stakerVRFSK, stakerVRFVK) <- ed25519VRFResource.use(
        _.createKeyPair(Entropy.fromUuid(UUID.randomUUID()), None).pure[F]
      )
      clock = SchedulerClock.Eval.make[F](SlotDuration, EpochLength, Instant.now())
      statsInterpreter = StatsInterpreter.Eval.make[F](statsDir)
      _ <- testConfigs.traverse { case (precision, amplitude, relativeStake) =>
        val vrfConfig =
          VrfConfig(lddCutoff = 10, precision = precision, baselineDifficulty = Ratio(1, 20), amplitude = amplitude)
        for {
          exp         <- ExpInterpreter.make[F](10000, precision)
          log1p       <- Log1pInterpreter.make[F](10000, 5)
          log1pCached <- Log1pInterpreter.makeCached[F](log1p)
          leaderElectionThreshold <- LeaderElectionValidation.Eval
            .make[F](vrfConfig, blake2b512Resource, exp, log1pCached)
            .pure[F]
          leaderElectionThresholdCached <- LeaderElectionValidation.Eval.makeCached[F](leaderElectionThreshold)
          threshold <- leaderElectionThresholdCached.getThreshold(relativeStake, vrfConfig.lddCutoff)
          _ <- statsInterpreter.write(
            TestName,
            Json.obj(
              "precision"     -> vrfConfig.precision.asJson,
              "amplitude"     -> vrfConfig.amplitude.toBigDecimal.asJson,
              "relativeStake" -> relativeStake.toBigDecimal.asJson,
              "threshold"     -> threshold.toBigDecimal.setScale(12, RoundingMode.HALF_EVEN).asJson
            )
          )
        } yield ()
      }
    } yield ()
  }
    .guarantee(
      Sync[F].delay(system.terminate()).flatMap(_ => Async[F].fromFuture(system.whenTerminated.pure[F])).void
    )

}
