package bifrost.contract

import java.time.Instant
import java.util.UUID

import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import bifrost.BifrostNodeViewHolder.{HIS, MP, MS, VL}
import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.srb.StateBoxRegistry
import bifrost.state.BifrostStateSpec.testSettings
import bifrost.transaction.box.{ArbitBox, StateBox, StateBoxSerializer}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import io.circe
import io.iohk.iodb.ByteArrayWrapper
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

  val path: Path = Path("/tmp/scorex/test-sbr")
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


  val uuid_2: UUID = UUID.randomUUID()

//  var history: BifrostHistory = generateHistory
  var sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(testSettings)

  property("SBR should update correctly for new state box with same UUID") {

    val block = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    sbr.update(block.id, uuid, sbox_1.id)

    //Should be able to access stateBoxID from sbr by UUID
    sbr.get(uuid).isSuccess shouldBe true
    assert(sbr.get(uuid).get._2 sameElements(sbox_1.id))

    //TODO Should be able to reconstruct stateBox from id
//    StateBoxSerializer.parseBytes(<get box from box id>).isSuccess shouldBe true


    Thread.sleep(1000)

    val block_2 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    sbr.update(block_2.id, uuid, sbox_2.id)

    //SBR should update correctly when replacing stateBoxID for same UUID
    sbr.get(uuid).isSuccess shouldBe true
    assert(sbr.get(uuid).get._2 sameElements(sbox_2.id))
  }

}
