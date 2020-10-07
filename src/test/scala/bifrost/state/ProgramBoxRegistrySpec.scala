package bifrost.state

import bifrost.modifier.ModifierId
import bifrost.modifier.box.StateBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.nodeView.NodeViewHolder
import bifrost.nodeView.NodeViewHolder.{HIS, MP, MS, VL}
import bifrost.settings.{AppSettings, StartupOpts}
import bifrost.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.Ints
import io.circe.syntax._
import org.scalatest.BeforeAndAfterAll
import scorex.util.encode.Base58

import scala.reflect.io.Path
import scala.util.Try
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import scorex.crypto.signatures.PublicKey

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

  val gs: (HIS, MS, VL, MP) = NodeViewHolder.initializeGenesis(testSettings)
  val history: HIS = gs._1
  val genesisState: MS = gs._2
  val gw: VL = gs._3

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
    val pbr_changes_1 = Some(ProgramRegistryChanges(Map(), Map(sboxOne.value -> Seq(BoxId(sboxOne.id)))))
    newState_1 = genesisState.applyChanges(ModifierId(Ints.toByteArray(1)), changes_1, None, pbr_changes_1).get

    assert(newState_1.registryLookup(sboxOne.value).get.head.hashBytes sameElements sboxOne.id)
    assert(newState_1.getProgramBox[StateBox](sboxOne.value).get.bytes sameElements sboxOne.bytes)

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set(sboxTwo))
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(BoxId(sboxOne.id))), Map(sboxTwo.value -> Seq(BoxId(sboxTwo.id)))))
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(2)), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxTwo.value).get.head.hashBytes sameElements sboxTwo.id)
    assert(newState_2.getProgramBox[StateBox](sboxTwo.value).get.bytes sameElements sboxTwo.bytes)

    val oldState = newState_2.rollbackTo(newState_1.version).get

    assert(oldState.registryLookup(sboxOne.value).get.head.hashBytes sameElements sboxOne.id)
  }

  property("BifrostState should tombstone program id in programBoxRegistry correctly") {

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set())
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(BoxId(sboxOne.id))), Map()))
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(3)), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxOne.value).isEmpty)
  }

  override def afterAll() {
    history.closeStorage()
    genesisState.closeStorage()
  }
}
