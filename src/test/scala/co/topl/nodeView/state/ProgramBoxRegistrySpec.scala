package co.topl.nodeView.state

import co.topl.modifier.ModifierId
import co.topl.nodeView.NodeViewHolder
import co.topl.nodeView.NodeViewHolder.{ HIS, MP, MS }
import co.topl.nodeView.state.box.StateBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.nodeView.state.box.{BoxId, StateBox}
import co.topl.settings.{AppSettings, StartupOpts}
import co.topl.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.Ints
import io.circe.syntax._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }
import scorex.crypto.signatures.PublicKey
import scorex.util.encode.Base58

import scala.reflect.io.Path
import scala.util.Try

class ProgramBoxRegistrySpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
  with ValidGenerators {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  private val settingsFilename = "src/test/resources/test.conf"
  lazy val testSettings: AppSettings = AppSettings.read(StartupOpts(Some(settingsFilename), None))

  val gs: (HIS, MS, MP) = NodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  val genesisState: MS = gs._2

  val pubKey: PublicKey25519Proposition =
    PublicKey25519Proposition(PublicKey @@ Base58.decode("6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ").get)

  val stateOne =
    s"""
       |{ "a": "0" }
     """.stripMargin.asJson

  val stateTwo =
    s"""
       |{"b": "1" }
     """.stripMargin.asJson

  val sboxOne: StateBox = StateBox(pubKey, 0L, programIdGen.sample.get, stateOne)
  val sboxTwo: StateBox = StateBox(pubKey, 1L, programIdGen.sample.get, stateTwo)

  var newState_1: State = null

  property("BifrostState should update programBoxRegistry with state box and rollback correctly") {

    val changes_1: StateChanges = StateChanges(Set(), Set(sboxOne))
    val pbr_changes_1 = Some(ProgramRegistryChanges(Map(), Map(sboxOne.value -> Seq(sboxOne.id))))
    newState_1 = genesisState.applyChanges(ModifierId(Ints.toByteArray(1)), changes_1, None, pbr_changes_1).get

    assert(newState_1.registryLookup(sboxOne.value).get.head == sboxOne.id)
    assert(newState_1.getProgramBox[StateBox](sboxOne.value).get.bytes sameElements sboxOne.bytes)

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set(sboxTwo))
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map(sboxTwo.value -> Seq(sboxTwo.id))))
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(2)), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxTwo.value).get.head == sboxTwo.id)
    assert(newState_2.getProgramBox[StateBox](sboxTwo.value).get.bytes sameElements sboxTwo.bytes)

    val oldState = newState_2.rollbackTo(newState_1.version).get

    assert(oldState.registryLookup(sboxOne.value).get.head == sboxOne.id)
  }

  property("BifrostState should tombstone program id in programBoxRegistry correctly") {

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set())
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map()))
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(3)), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxOne.value).isEmpty)
  }

  override def afterAll() {
    history.closeStorage()
    genesisState.closeStorage()
  }
}
