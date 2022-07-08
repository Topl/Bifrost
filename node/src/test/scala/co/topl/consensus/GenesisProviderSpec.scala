package co.topl.consensus

import co.topl.attestation.Address
import co.topl.consensus.GenesisProvider.Strategies
import co.topl.modifier.box.ArbitBox
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer, TransferTransaction}
import co.topl.nodeView.ValidTransactionGenerators
import co.topl.settings.AppSettings
import co.topl.settings.GenesisStrategies.{FromBlockJson, Generated}
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.{Int128, NetworkType, TestSettings}
import io.circe.syntax.EncoderOps
import io.circe.{parser, Json, JsonObject}
import org.scalacheck.Gen
import org.scalatest.EitherValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class GenesisProviderSpec
    extends AnyPropSpecLike
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with TestSettings
    with ValidTransactionGenerators
    with Matchers {

  property("should be able to construct a genesis block from generated inputs") {
    forAll(nonEmptySetAddressGen) { addresses =>
      val (balances, initialDifficulty, blockVersion) = (
        Gen.choose(1, Int.MaxValue).sample.get.toLong,
        positiveLongGen.sample.get,
        Gen.choose[Byte](1, Byte.MaxValue).sample.get
      )
      val genesis = GenesisProvider.construct(addresses, balances, initialDifficulty, blockVersion)

      val blockTotalStake = genesis.block.transactions
        .collect { case transaction: TransferTransaction[_, _] =>
          transaction.newBoxes.collect { case box: ArbitBox => box.value.quantity }.toSeq
        }
        .flatten
        .sum

      genesis.block.height shouldBe 1L
      genesis.state.totalStake shouldBe addresses.size * balances
      genesis.state.totalStake shouldBe blockTotalStake
      genesis.block.difficulty shouldBe genesis.block.difficulty
    }
  }

  property(
    "should be able to read in json files and verify checksum, and the encoded json from the " +
    "generated block should be the same as the one from file"
  ) {
    val jsonSettings = Seq(
      GenesisSpecSetup.toplnetJsonGenesisSettings,
      GenesisSpecSetup.valhallaJsonGenesisSettings,
      GenesisSpecSetup.helJsonGenesisSettings
    )
    jsonSettings.map { jsonSetting =>
      val genesis: ConsensusHolder.Genesis =
        new GenesisProvider(protocolVersioner.applicable(1).blockVersion, Set[Address]())
          .fetchGenesis(GenesisSpecSetup.genesisFromJsonSettings(jsonSetting.fromJson))(jsonSetting.networkPrefix)
          .value

      val jsonFileSource = scala.io.Source.fromResource(jsonSetting.fromJson.genesisFile)
      val jsonFromFile = parser.parse(jsonFileSource.mkString).getOrElse(Json.fromJsonObject(JsonObject.empty))
      jsonFileSource.close()

      jsonFromFile shouldEqual genesis.block.asJson
    }
  }

  property("should successfully generate a genesis block and consensus view") {
    forAll(nonEmptySetAddressGen) { addresses =>
      val expectedCoinAmount = Int128(
        addresses.size * GenesisSpecSetup.genesisGenSettings.application.genesis.generated
          .map(_.balanceForEachParticipant)
          .get
      )

      val genesis: ConsensusHolder.Genesis =
        new GenesisProvider(protocolVersioner.applicable(1).blockVersion, addresses)
          .fetchGenesis(GenesisSpecSetup.genesisGenSettings)
          .value

      val actualCoinTotals = genesis.block.transactions match {
        case Seq(arbitTx: ArbitTransfer[_], polyTx: PolyTransfer[_]) =>
          val arbitCoinTotal = arbitTx.coinOutput.map(_.value.quantity).sum
          val polyCoinTotal =
            arbitTx.feeChangeOutput.value.quantity +
            polyTx.feeChangeOutput.value.quantity +
            polyTx.coinOutput.map(_.value.quantity).sum

          (arbitCoinTotal, polyCoinTotal)
      }

      actualCoinTotals._1 shouldBe expectedCoinAmount
      actualCoinTotals._2 shouldBe expectedCoinAmount
    }
  }

  object GenesisSpecSetup {
    case class GenesisTotals(coinTotal: Int128, arbitTotal: Int128, polyTotal: Int128)
    case class blockJsonSetting(fromJson: Strategies.FromBlockJson, networkPrefix: NetworkPrefix)

    val genesisGenSettings: AppSettings = settings.copy(application =
      settings.application.copy(genesis = settings.application.genesis.copy(strategy = Generated))
    )

    val (toplnetJsonGenesisSettings, valhallaJsonGenesisSettings, helJsonGenesisSettings) =
      (
        blockJsonSetting(
          Strategies.FromBlockJson(
            "toplnet-genesis.json"
          ),
          NetworkType.Mainnet.netPrefix
        ),
        blockJsonSetting(
          Strategies.FromBlockJson(
            "valhalla-genesis.json"
          ),
          NetworkType.ValhallaTestnet.netPrefix
        ),
        blockJsonSetting(
          Strategies.FromBlockJson(
            "hel-genesis.json"
          ),
          NetworkType.HelTestnet.netPrefix
        )
      )

    def genesisFromJsonSettings(fromJson: GenesisProvider.Strategies.FromBlockJson): AppSettings =
      settings.copy(application =
        settings.application.copy(genesis =
          settings.application.genesis.copy(strategy = FromBlockJson, fromBlockJson = Some(fromJson))
        )
      )
  }
}
