package co.topl.consensus

import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.utils.NetworkType._
import co.topl.utils.{InMemoryKeyFileTestHelper, Int128, NodeGenerators}
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpecLike
import org.scalatest.{BeforeAndAfterAll, EitherValues}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.slf4j.Logger

class GenesisProviderSpec
    extends AnyPropSpecLike
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with EitherValues
    with MockFactory
    with InMemoryKeyFileTestHelper
    with BeforeAndAfterAll
    with NodeGenerators
    with Matchers {

  implicit private def implicitLogger: Logger = logger.underlying

//  object GenesisSpecSetup {
//    case class GenesisTotals(coinTotal: Int128, arbitTotal: Int128, polyTotal: Int128)
//
//    val genesisGenSettings: GenesisGenerationSettings = settings.forging.genesis.map(_.generated).head
//
//    val (toplnetJsonGenesisSettings, valhallaJsonGenesisSettings, helJsonGenesisSettings) =
//      (
//        GenesisFromBlockJsonSettings(
//          "node/src/main/resources/toplnet-genesis.json",
//          "228AWnLyoHdV3hzNaJmABsmB4VoS9rxPREA3AofbZnJob"
//        ),
//        GenesisFromBlockJsonSettings(
//          "node/src/main/resources/valhalla-genesis.json",
//          "wgUeiENYY32eC5T6WM2UiqAf6Ayba2tFNtvFkgn999iG"
//        ),
//        GenesisFromBlockJsonSettings(
//          "node/src/main/resources/hel-genesis.json",
//          "vKjyX77HLRUiihjWofSsacNEdDGMaJpNJTQMXkRyJkP2"
//        )
//      )
//
//    val keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion
//
//    val privateKeyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] =
//      KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](None)(
//        PrivateTestnet.netPrefix,
//        PrivateKeyCurve25519.secretGenerator,
//        keyfileCurve25519Companion
//      )
//
//    privateKeyRing
//      .generateNewKeyPairs(settings.forging.genesis.map(_.generated).get.numberOfParticipants, None)
//      .map(keys => keys.map(_.publicImage.address))
//  }

  property("read in json files and verify checksum; block->checksum->Json->read Json->check checksum") {
    // test
  }

  property("successfully generate a genesis block and consensus view") {
    forAll(genesisGenerationSettingsGen) { genesisSettings =>
      val expectedCoinAmount = Int128(genesisSettings * genesisSettings.balanceForEachParticipant)

      val genesis =
        GenesisProvider
          .generatedGenesisProvider(
            GenesisSpecSetup.privateKeyRing.addresses,
            genesisSettings.genesisApplicationVersion.blockByte
          )
          .executeStrategy(genesisSettings)

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
}
