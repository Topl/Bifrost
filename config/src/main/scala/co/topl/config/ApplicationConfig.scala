package co.topl.config

import co.topl.brambl.models.LockAddress
import co.topl.consensus.models.{BlockId, StakingAddress}
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits._
import co.topl.proto.node.NodeConfig
import monocle.macros.Lenses

import scala.concurrent.duration.{FiniteDuration, SECONDS}

// $COVERAGE-OFF$
@Lenses
case class ApplicationConfig(
  bifrost: ApplicationConfig.Bifrost,
  genus:   ApplicationConfig.Genus,
  kamon:   ApplicationConfig.Kamon
)

object ApplicationConfig {

  @Lenses
  case class Bifrost(
    data:        Bifrost.Data,
    staking:     Bifrost.Staking,
    p2p:         Bifrost.P2P,
    rpc:         Bifrost.RPC,
    mempool:     Bifrost.Mempool,
    bigBang:     Bifrost.BigBang,
    protocols:   Map[Slot, Bifrost.Protocol],
    cache:       Bifrost.Cache,
    ntp:         Bifrost.Ntp,
    versionInfo: Bifrost.VersionInfo
  )

  object Bifrost {

    @Lenses
    case class Data(directory: String, databaseType: String)

    @Lenses
    case class Staking(directory: String, rewardAddress: LockAddress, stakingAddress: Option[StakingAddress])

    @Lenses
    case class P2P(
      bindHost:          String,
      bindPort:          Int,
      publicHost:        Option[String],
      publicPort:        Option[Int],
      knownPeers:        List[KnownPeer],
      networkProperties: NetworkProperties
    )

    case class NetworkProperties(
      useHostNames:                         Boolean = false,
      pingPongInterval:                     FiniteDuration = FiniteDuration(90, SECONDS),
      expectedSlotsPerBlock:                Double = 15.0, // TODO shall be calculated?
      maxPerformanceDelayInSlots:           Double = 2.0,
      remotePeerNoveltyInExpectedBlocks:    Double = 2.0,
      minimumBlockProvidingReputationPeers: Int = 2,
      minimumPerformanceReputationPeers:    Int = 2,
      minimumRequiredReputation:            Double = 0.66,
      // any non-new peer require that reputation to be hot
      minimumBlockProvidingReputation: Double = 0.15,
      minimumEligibleColdConnections:  Int = 50,
      maximumEligibleColdConnections:  Int = 100,
      clearColdIfNotActiveForInMs:     Long = 7 * 24 * 60 * 60 * 1000, // 7 days
      minimumHotConnections:           Int = 7,
      maximumWarmConnections:          Int = 12,
      warmHostsUpdateEveryNBlock:      Double = 4.0,
      p2pTrackInterval:                FiniteDuration = FiniteDuration(10, SECONDS),
      // we could try to connect to remote peer again after
      // closeTimeoutFirstDelayInMs * {number of closed connections in last closeTimeoutWindowInMs} ^ 2
      closeTimeoutFirstDelayInMs: Long = 1000,
      closeTimeoutWindowInMs:     Long = 1000 * 60 * 60 * 24, // 1 day
      aggressiveP2P:              Boolean = true, // always try to found new good remote peers
      aggressiveP2PCount:         Int = 1 // how many new connection will be opened
    )

    case class KnownPeer(host: String, port: Int)

    @Lenses
    case class RPC(bindHost: String, bindPort: Int)

    @Lenses
    case class Mempool(defaultExpirationSlots: Long, protection: MempoolProtection = MempoolProtection())

    case class MempoolProtection(
      enabled: Boolean = false,
      // do not perform checks if number of transactions in mempool less than that value
      noCheckIfLess: Long = 10,
      // during semantic check we will include all transactions from memory pool in context
      // if total tx count in memory pool is less that that value
      useMempoolForSemanticIfLess: Long = 100
    )

    sealed abstract class BigBang

    object BigBangs {

      @Lenses
      case class Private(
        timestamp:        Long = System.currentTimeMillis() + 5_000L,
        stakerCount:      Int,
        stakes:           Option[List[BigInt]],
        localStakerIndex: Option[Int]
      ) extends BigBang

      @Lenses
      case class Public(
        genesisId:  BlockId,
        sourcePath: String
      ) extends BigBang
    }

    @Lenses
    case class Protocol(
      minAppVersion:              String,
      fEffective:                 Ratio,
      vrfLddCutoff:               Int,
      vrfPrecision:               Int,
      vrfBaselineDifficulty:      Ratio,
      vrfAmplitude:               Ratio,
      chainSelectionKLookback:    Long,
      slotDuration:               FiniteDuration,
      forwardBiasedSlotWindow:    Slot,
      operationalPeriodsPerEpoch: Long,
      kesKeyHours:                Int,
      kesKeyMinutes:              Int,
      epochLengthOverride:        Option[Long]
    ) {

      val chainSelectionSWindow: Long =
        (Ratio(chainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

      val epochLength: Long =
        epochLengthOverride.getOrElse(((Ratio(chainSelectionKLookback) * fEffective.inverse) * 3).round.toLong)

      val operationalPeriodLength: Long =
        epochLength / operationalPeriodsPerEpoch

      val vrfCacheSize: Long =
        operationalPeriodLength * 4

      def nodeConfig(slot: Slot): NodeConfig = NodeConfig(
        slot = slot,
        slotDurationMillis = slotDuration.toMillis,
        epochLength = epochLength
      )

      def validation: Either[String, Unit] =
        for {
          _ <- Either.cond(epochLength % 3L == 0, (), s"Epoch length=$epochLength must be divisible by 3")
          _ <- Either.cond(
            epochLength % operationalPeriodsPerEpoch == 0,
            (),
            s"Epoch length=$epochLength must be divisible by $operationalPeriodsPerEpoch"
          )
        } yield ()
    }

    @Lenses
    case class Cache(
      parentChildTree:         Cache.CacheConfig,
      slotData:                Cache.CacheConfig,
      headers:                 Cache.CacheConfig,
      bodies:                  Cache.CacheConfig,
      transactions:            Cache.CacheConfig,
      spendableBoxIds:         Cache.CacheConfig,
      epochBoundaries:         Cache.CacheConfig,
      operatorStakes:          Cache.CacheConfig,
      registrations:           Cache.CacheConfig,
      blockHeightTree:         Cache.CacheConfig,
      eligibilities:           Cache.CacheConfig,
      epochData:               Cache.CacheConfig,
      registrationAccumulator: Cache.CacheConfig,
      containsCacheSize:       Long = 16384
    )

    object Cache {

      @Lenses
      case class CacheConfig(maximumEntries: Long, ttl: Option[FiniteDuration])
    }

    @Lenses
    case class Ntp(server: String, refreshInterval: FiniteDuration, timeout: FiniteDuration)

    @Lenses
    case class VersionInfo(enable: Boolean, uri: String, period: FiniteDuration)

  }

  @Lenses
  case class Genus(
    enable:            Boolean,
    orientDbDirectory: String,
    orientDbPassword:  String
  )

  @Lenses
  case class Kamon(enable: Boolean)
}
// $COVERAGE-ON$
