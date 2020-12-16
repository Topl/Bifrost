package co.topl.nodeView.state

import co.topl.modifier.ModifierId
import co.topl.nodeView.state.box.StateBox
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Ints
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.signatures.PublicKey
import scorex.util.encode.Base58

import scala.reflect.io.Path
import scala.util.Try

class ProgramBoxRegistrySpec extends StateSpec {

  val path: Path = Path("/tmp/bifrost/test-data")
  Try(path.deleteRecursively())

  val state: State = createState()

  val pubKey: PublicKey25519Proposition = propositionGen.sample.get

  val stateOne: Json =
    s"""
       |{ "a": "0" }
     """.stripMargin.asJson

  val stateTwo: Json =
    s"""
       |{"b": "1" }
     """.stripMargin.asJson

  val sboxOne: StateBox = StateBox(pubKey, 0L, programIdGen.sample.get, stateOne)
  val sboxTwo: StateBox = StateBox(pubKey, 1L, programIdGen.sample.get, stateTwo)

  var newState_1: State = null

  property("BifrostState should update programBoxRegistry with state box and rollback correctly") {

    val changes_1: StateChanges = StateChanges(Set(), Set(sboxOne))
    val pbr_changes_1 = Some(ProgramRegistryChanges(Map(), Map(sboxOne.value -> Seq(sboxOne.id))))
    newState_1 = state.applyChanges(
      ModifierId(specificLengthBytesGen(ModifierId.size).sample.get), changes_1, None, pbr_changes_1).get

    assert(newState_1.registryLookup(sboxOne.value).get.head == sboxOne.id)
    assert(newState_1.getProgramBox[StateBox](sboxOne.value).get.bytes sameElements sboxOne.bytes)

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set(sboxTwo))
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map(sboxTwo.value -> Seq(sboxTwo.id))))
    val newState_2 = newState_1.applyChanges(ModifierId(specificLengthBytesGen(ModifierId.size).sample.get), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxTwo.value).get.head == sboxTwo.id)
    assert(newState_2.getProgramBox[StateBox](sboxTwo.value).get.bytes sameElements sboxTwo.bytes)

    val oldState = newState_2.rollbackTo(newState_1.version).get

    assert(oldState.registryLookup(sboxOne.value).get.head == sboxOne.id)
  }

  property("BifrostState should tombstone program id in programBoxRegistry correctly") {

    val changes_2: StateChanges = StateChanges(Set(sboxOne.id), Set())
    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map()))
    val newState_2 = newState_1.applyChanges(ModifierId(specificLengthBytesGen(ModifierId.size).sample.get), changes_2, None, pbr_changes_2).get

    assert(newState_2.registryLookup(sboxOne.value).isEmpty)
  }

  override def afterAll() {
    state.closeStorage()
  }
}
