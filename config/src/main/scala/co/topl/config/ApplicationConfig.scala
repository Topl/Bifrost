package co.topl.config

import co.topl.brambl.models.LockAddress
import co.topl.consensus.models.BlockId
import co.topl.models.Slot
import co.topl.models.utility.Ratio
import co.topl.numerics.implicits.Ops
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
    data:      Bifrost.Data,
    staking:   Bifrost.Staking,
    p2p:       Bifrost.P2P,
    rpc:       Bifrost.RPC,
    mempool:   Bifrost.Mempool,
    bigBang:   Bifrost.BigBang,
    protocols: Map[Slot, Bifrost.Protocol],
    cache:     Bifrost.Cache,
    ntp:       Bifrost.Ntp
  )

  object Bifrost {

    @Lenses
    case class Data(directory: String)

    @Lenses
    case class Staking(directory: String, rewardAddress: LockAddress)

    @Lenses
    case class P2P(
      bindHost:          String,
      bindPort:          Int,
      publicHost:        String,
      publicPort:        Int,
      knownPeers:        List[KnownPeer],
      networkProperties: NetworkProperties
    )

    case class NetworkProperties(
      legacyNetwork:                        Boolean = false,
      pingPongInterval:                     FiniteDuration = FiniteDuration(90, SECONDS),
      expectedSlotsPerBlock:                Double = 5.0, // TODO shall be calculated?
      maxPerformanceDelayInSlots:           Double = 2.0,
      remotePeerNoveltyInExpectedBlocks:    Double = 2.0,
      minimumBlockProvidingReputationPeers: Int = 2,
      minimumPerformanceReputationPeers:    Int = 1,
      minimumRequiredReputation:            Double = 0.66,
      minimumHotConnections:                Int = 3,
      minimumWarmConnections:               Int = 6,
      maximumWarmConnections:               Int = 12,
      warmHostsUpdateEveryNBlock:           Double = 4.0,
      // we could try to connect to remote peer again after
      // closeTimeoutFirstDelayInMs * {number of closed connections in last closeTimeoutWindowInMs} ^ 2
      closeTimeoutFirstDelayInMs: Long = 1000,
      closeTimeoutWindowInMs:     Long = 1000 * 60 * 60 * 24 // 1 day
    )

    case class KnownPeer(host: String, port: Int)

    @Lenses
    case class RPC(bindHost: String, bindPort: Int)

    @Lenses
    case class Mempool(defaultExpirationSlots: Long)
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
      kesKeyMinutes:              Int
    ) {

      val chainSelectionSWindow: Long =
        (Ratio(chainSelectionKLookback, 4L) * fEffective.inverse).round.toLong

      val epochLength: Long =
        chainSelectionKLookback * 6

      val operationalPeriodLength: Long =
        epochLength / operationalPeriodsPerEpoch

      val vrfCacheSize: Long =
        operationalPeriodLength * 4

      def nodeConfig(slot: Slot): NodeConfig = NodeConfig(
        slot = slot,
        slotDurationMillis = slotDuration.toMillis,
        epochLength = epochLength
      )
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
      registrationAccumulator: Cache.CacheConfig
    )

    object Cache {

      @Lenses
      case class CacheConfig(maximumEntries: Long, ttl: Option[FiniteDuration])
    }

    @Lenses
    case class Ntp(server: String, refreshInterval: FiniteDuration, timeout: FiniteDuration)

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
