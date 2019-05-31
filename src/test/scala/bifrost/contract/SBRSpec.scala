package bifrost.contract

import java.util.UUID

import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import bifrost.BifrostNodeViewHolder.{HIS, MP, MS, VL}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.srb.StateBoxRegistry
import bifrost.state.BifrostStateSpec.testSettings
import bifrost.transaction.box.{StateBox, StateBoxSerializer}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import io.circe
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.Try

class SBRSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {

  val settingsFilename = "testSettings.json"
  lazy val testSettings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

//  val gs: (HIS, MS, VL, MP) = BifrostNodeViewHolder.initializeGenesis(testSettings)
//  val history: HIS = gs._1
//  var genesisState: MS = gs._2
//  var gw: VL = gs._3

//  val pubKey: PublicKey25519Proposition = PublicKey25519Proposition("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ".toArray[Byte])

  val pubKey: PublicKey25519Proposition = PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte))
  val sbox_1: StateBox = StateBox(pubKey, 0L, Seq("a"), true)
  val sbox_2: StateBox = StateBox(pubKey, 1L, Seq("b"), true)

  val uuid: UUID = UUID.randomUUID()

//  var history: BifrostHistory = generateHistory
  var sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(testSettings)

  property("SBR should update correctly for new state box with same UUID") {

    sbr.update(uuid, StateBoxSerializer.toBytes(sbox_1))
    sbr.get(uuid).isSuccess shouldBe true
//    StateBoxSerializer.parseBytes(sbr.get(uuid).get._2).isSuccess shouldBe true
    StateBoxSerializer.parseBytes(sbr.get(uuid).get._2).get == sbox_1 shouldBe true

    sbr.update(uuid, StateBoxSerializer.toBytes(sbox_2))
    StateBoxSerializer.parseBytes(sbr.get(uuid).get._2).isSuccess shouldBe true
    StateBoxSerializer.parseBytes(sbr.get(uuid).get._2).get == sbox_2 shouldBe true
  }

}
