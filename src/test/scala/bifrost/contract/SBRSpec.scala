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
  val sboxOne: StateBox = StateBox(pubKey, 0L, Seq("a"), true)
  val sboxTwo: StateBox = StateBox(pubKey, 1L, Seq("b"), true)

  val uuid: UUID = UUID.nameUUIDFromBytes(sboxOne.id)
  val uuidTwo: UUID = UUID.nameUUIDFromBytes(sboxTwo.id)

//  var history: BifrostHistory = generateHistory
  var sbr: StateBoxRegistry = StateBoxRegistry.readOrGenerate(testSettings)

  property("SBR should update correctly for new state box with same UUID") {

    val block = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    sbr.update(block.id, uuid, sboxOne.id)

    //Should be able to access stateBoxID from sbr by UUID
    sbr.get(uuid).isSuccess shouldBe true
    assert(sbr.get(uuid).get._2 sameElements(sboxOne.id))

    Thread.sleep(1000)

    val block_2 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    sbr.update(block_2.id, uuid, sboxTwo.id)

    //SBR should update correctly when replacing stateBoxID for same UUID
    sbr.get(uuid).isSuccess shouldBe true
    assert(sbr.get(uuid).get._2 sameElements(sboxTwo.id))
  }

  property("SBR should deterministically generate a new UUID for a new state box") {

    Thread.sleep(1000)
    val block_3 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    val sbox_3: StateBox = StateBox(pubKey, 2L, Seq("c"), true)
    val uuidAndBoxID = sbr.insertNewStateBox(block_3.id, sbox_3.id)
    uuidAndBoxID.isSuccess shouldBe true
    //    assert(uuidAndBoxIDTwo.get._1 == new UUID(0L, 0L))
    assert(uuidAndBoxID.get._1 == UUID.nameUUIDFromBytes(sbox_3.id))
    assert(uuidAndBoxID.get._2 sameElements sbox_3.id)

    //Test to make sure counters update correctly
    Thread.sleep(1000)
    val block_4 = BifrostBlock(
      Array.fill(BifrostBlock.SignatureLength)(-1: Byte),
      Instant.now().toEpochMilli,
      ArbitBox(PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(0: Byte)), 0L, 0L),
      Signature25519(Array.fill(BifrostBlock.SignatureLength)(0: Byte)),
      Seq(), 10L)

    val sbox_4: StateBox = StateBox(pubKey, 3L, Seq("d"), true)
    val uuidAndBoxIDTwo = sbr.insertNewStateBox(block_4.id, sbox_4.id)
    uuidAndBoxIDTwo.isSuccess shouldBe true
//    assert(uuidAndBoxIDTwo.get._1 == new UUID(0L, 1L))
    assert(uuidAndBoxIDTwo.get._1 == UUID.nameUUIDFromBytes(sbox_4.id))
    assert(uuidAndBoxIDTwo.get._2 sameElements sbox_4.id)
  }
}
