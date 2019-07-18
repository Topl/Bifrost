package bifrost.state

import java.util.UUID

import bifrost.BifrostNodeViewHolder.{HIS, MP, MS, VL}
import bifrost.forging.ForgingSettings
import bifrost.transaction.box.StateBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.{BifrostGenerators, BifrostNodeViewHolder, ValidGenerators}
import com.google.common.primitives.Ints
import io.circe
import io.circe.syntax._
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.reflect.io.Path
import scala.util.Try

class SBRSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  val settingsFilename = "testSettings.json"
  lazy val testSettings: ForgingSettings = new ForgingSettings {
    override val settingsJSON: Map[String, circe.Json] = settingsFromFile(settingsFilename)
  }

  val path: Path = Path("/tmp/scorex/test-data")
  Try(path.deleteRecursively())

  val gs: (HIS, MS, VL, MP) = BifrostNodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  var genesisState: MS = gs._2
  var gw: VL = gs._3

  val pubKey: PublicKey25519Proposition = PublicKey25519Proposition(Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)

  val stateOne =
    s"""
       |{ "a": "0" }
     """.stripMargin.asJson

  val stateTwo =
    s"""
       |{"b": "1" }
     """.stripMargin.asJson

  val sboxOneWithoutUUID: StateBox = StateBox(pubKey, 0L, null, stateOne, true)
  val sboxTwoWithoutUUID: StateBox = StateBox(pubKey, 1L, null, stateTwo, true)

  val uuid: UUID = UUID.nameUUIDFromBytes(sboxOneWithoutUUID.id)

  val sboxOne: StateBox = StateBox(pubKey, 0L, uuid, stateOne, true)
  val sboxTwo: StateBox = StateBox(pubKey, 1L, uuid, stateTwo, true)

  var newState_1: BifrostState = null

  assert(sboxOne.value == uuid)

  property("BifrostState should update sbr with state box and rollback correctly") {

    val changes_1: BifrostStateChanges = BifrostStateChanges(Set(), Set(sboxOne), 0L)
    newState_1 = genesisState.applyChanges(changes_1, Ints.toByteArray(1)).get

    assert(newState_1.sbr.getBoxId(uuid).get sameElements sboxOne.id)
    assert(newState_1.sbr.getBox(uuid).get.bytes sameElements sboxOne.bytes)

    val changes_2: BifrostStateChanges = BifrostStateChanges(Set(sboxOne.id), Set(sboxTwo), 0L)
    val newState_2 = newState_1.applyChanges(changes_2, Ints.toByteArray(2)).get

    assert(newState_2.sbr.getBoxId(uuid).get sameElements sboxTwo.id)
    assert(newState_2.sbr.getBox(uuid).get.bytes sameElements sboxTwo.bytes)

    val oldState = newState_2.rollbackTo(newState_1.version).get

    assert(oldState.sbr.getBoxId(sboxOne.value).get sameElements sboxOne.id)
  }

  property("BifrostState should tombstone uuid in sbr correctly") {

    val changes_2: BifrostStateChanges = BifrostStateChanges(Set(sboxOne.id), Set(), 0L)
    val newState_2 = newState_1.applyChanges(changes_2, Ints.toByteArray(3)).get

    assert(!newState_2.sbr.getBoxId(sboxOne.value).isDefined)
  }

}
