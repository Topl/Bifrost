package co.topl.node

import cats.implicits._
import co.topl.consensus.LeaderElectionValidation.VrfConfig
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

import scala.concurrent.duration._

/**
 * Note: These values will be made configurable in a future ticket
 */
object ProtocolConfiguration {

  val fEffective: Ratio = Ratio(15, 100)

  implicit val vrfConfig: VrfConfig =
    VrfConfig(lddCutoff = 15, precision = 40, baselineDifficulty = Ratio(1, 20), amplitude = Ratio(1, 2))

  val ChainSelectionKLookback: Long = 50
  val SlotDuration: FiniteDuration = 100.milli
  val OperationalPeriodsPerEpoch: Long = 2L

  val ChainSelectionSWindow: Long =
    (Ratio(ChainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

  val EpochLength: Long =
    ChainSelectionSWindow * 6

  val OperationalPeriodLength: Long =
    EpochLength / OperationalPeriodsPerEpoch

  require(
    EpochLength % OperationalPeriodLength === 0L,
    "EpochLength must be evenly divisible by OperationalPeriodLength"
  )

  val KesKeyHeight: (Int, Int) =
    (9, 9)

  val OperatorRegistrationMaxLength: Long =
    OperationalPeriodLength * Ratio(2, 1).pow(KesKeyHeight._1 + KesKeyHeight._2).round.toLong

}
