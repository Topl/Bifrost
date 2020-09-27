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
import scorex.crypto.encode.Base58

import scala.reflect.io.Path
import scala.util.Try
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec

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

  val sboxOneWithoutUUID: StateBox = StateBox(pubKey, 0L, null, stateOne)
  val sboxTwoWithoutUUID: StateBox = StateBox(pubKey, 1L, null, stateTwo)

  val progId: ProgramId = ProgramId.create()

  val sboxOne: StateBox = StateBox(pubKey, 0L, progId, stateOne)
  val sboxTwo: StateBox = StateBox(pubKey, 1L, progId, stateTwo)

  var newState_1: State = null

  assert(sboxOne.value == progId)

  property("BifrostState should update programBoxRegistry with state box and rollback correctly") {

    val changes_1: StateChanges = StateChanges(Set(), Set(sboxOne))
    val pbr_1 = genesisState.pbrOpt.get.update(ModifierId(Ints.toByteArray(1)), Map(), Map(progId -> Seq(BoxId(sboxOne.id)))).toOption
    newState_1 = genesisState.applyChanges(ModifierId(Ints.toByteArray(1)), changes_1, None, pbr_1).get

    assert(newState_1.registryLookup(progId).get.head.hashBytes sameElements sboxOne.id)
    assert(newState_1.getProgramBox[StateBox](progId).get.bytes sameElements sboxOne.bytes)

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set(sboxTwo))
    val pbr_2 = genesisState.pbrOpt.get.update(ModifierId(Ints.toByteArray(2)), Map(progId -> Seq(BoxId(sboxOne.id))), Map(progId -> Seq(BoxId(sboxTwo.id)))).toOption
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(2)), changes_2, None, pbr_2).get

    assert(newState_2.registryLookup(progId).get.head.hashBytes sameElements sboxTwo.id)
    assert(newState_2.getProgramBox[StateBox](progId).get.bytes sameElements sboxTwo.bytes)

    val oldState = newState_2.rollbackTo(newState_1.version).get

    assert(oldState.registryLookup(sboxOne.value).get.head.hashBytes sameElements sboxOne.id)
  }

  property("BifrostState should tombstone program id in programBoxRegistry correctly") {

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set())
    val pbr_2 = genesisState.pbrOpt.get.update(ModifierId(Ints.toByteArray(3)), Map(progId -> Seq(BoxId(sboxOne.id))), Map()).toOption
    val newState_2 = newState_1.applyChanges(ModifierId(Ints.toByteArray(3)), changes_2, None, pbr_2).get

    assert(newState_2.registryLookup(sboxOne.value).isEmpty)
  }

  override def afterAll() {
    history.closeStorage()
    genesisState.closeStorage()
  }
}
