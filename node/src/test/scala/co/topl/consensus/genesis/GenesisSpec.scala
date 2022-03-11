package co.topl.consensus.genesis

import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.{GenesisFromBlockJsonSettings, GenesisGenerationSettings}
import co.topl.utils.NetworkType._
import co.topl.utils.{Int128, NodeGenerators}
import org.scalatest.Inspectors
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class GenesisSpec extends AnyFreeSpec with Inspectors with NodeGenerators {

  case class GenesisTotals(coinTotal: Int128, arbitTotal: Int128, polyTotal: Int128)

  val genesisGenSettings: GenesisGenerationSettings = settings.forging.genesis.flatMap(_.generated).head

  val (toplnetJsonGenesisSettings, valhallaJsonGenesisSettings, helJsonGenesisSettings) =
    (
      GenesisFromBlockJsonSettings(
        "node/src/main/resources/toplnet-genesis.json",
        "228AWnLyoHdV3hzNaJmABsmB4VoS9rxPREA3AofbZnJob"
      ),
      GenesisFromBlockJsonSettings(
        "node/src/main/resources/valhalla-genesis.json",
        "wgUeiENYY32eC5T6WM2UiqAf6Ayba2tFNtvFkgn999iG"
      ),
      GenesisFromBlockJsonSettings(
        "node/src/main/resources/hel-genesis.json",
        "vKjyX77HLRUiihjWofSsacNEdDGMaJpNJTQMXkRyJkP2"
      )
    )

  val keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion

  val privateKeyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](None)(
      PrivateTestnet.netPrefix,
      PrivateKeyCurve25519.secretGenerator,
      keyfileCurve25519Companion
    )

  privateKeyRing
    .generateNewKeyPairs(settings.forging.genesis.flatMap(_.generated).get.numTestnetAccts, None)
    .map(keys => keys.map(_.publicImage.address))

  "arbit and poly total should equal the coin total of all addresses except for valhalla" in {
    forAll(
      List(
        ToplnetGenesis,
        ValhallaGenesis,
        HelGenesis,
        GeneratedGenesis(privateKeyRing.addresses, genesisGenSettings)(Mainnet.netPrefix),
        GeneratedGenesis(privateKeyRing.addresses, genesisGenSettings)(ValhallaTestnet.netPrefix),
        GeneratedGenesis(privateKeyRing.addresses, genesisGenSettings)(HelTestnet.netPrefix),
        GeneratedGenesis(privateKeyRing.addresses, genesisGenSettings)(PrivateTestnet.netPrefix)
      )
    ) { genesisProvider: GenesisProvider =>
      genesisProvider match {
        case vg: ValhallaGenesis.type =>
          val genesisTotals = getGenesisTotals(vg)
          genesisTotals.arbitTotal shouldBe genesisTotals.coinTotal - genesisProvider.members.head._2
          genesisTotals.polyTotal shouldBe genesisTotals.coinTotal + genesisProvider.members.head._2

        case gp =>
          val genesisTotals = getGenesisTotals(gp)
          genesisTotals.arbitTotal shouldBe genesisTotals.coinTotal
          genesisTotals.polyTotal shouldBe genesisTotals.coinTotal
      }
    }
  }

  "block Json in node/src/main/resources should create sasme genesis blocks as the ToplnetGenesis, ValhallaGenesis, " +
  "and HelGenesis does" in {
    val (toplnetFromJson, valhallaFromJson, helFromJson, toplnetGenesis, valhallaGenesis, helGenesis) = (
      GenesisFromBlockJson(toplnetJsonGenesisSettings, Mainnet).getGenesisBlock,
      GenesisFromBlockJson(valhallaJsonGenesisSettings, ValhallaTestnet).getGenesisBlock,
      GenesisFromBlockJson(helJsonGenesisSettings, HelTestnet).getGenesisBlock,
      ToplnetGenesis.getGenesisBlock,
      ValhallaGenesis.getGenesisBlock,
      HelGenesis.getGenesisBlock
    )
    toplnetFromJson shouldEqual toplnetGenesis
    valhallaFromJson shouldEqual valhallaGenesis
    helFromJson shouldEqual helGenesis
  }

  def getGenesisTotals(genesisProvider: GenesisProvider): GenesisTotals = {

    val totalCoins: Int128 = genesisProvider.members.values.sum

    val (arbitTotal, polyTotal) = genesisProvider.getGenesisBlock
      .map { case (block, _) =>
        block.transactions match {
          case Seq(arbitTx: ArbitTransfer[_], polyTx: PolyTransfer[_]) =>
            val arbitCoinTotal = arbitTx.coinOutput.map(_.value.quantity).sum
            val polyCoinTotal =
              arbitTx.feeChangeOutput.value.quantity +
              polyTx.feeChangeOutput.value.quantity +
              polyTx.coinOutput.map(_.value.quantity).sum
            (arbitCoinTotal, polyCoinTotal)
        }
      }
      .toOption
      .get

    GenesisTotals(totalCoins, arbitTotal, polyTotal)
  }

}
