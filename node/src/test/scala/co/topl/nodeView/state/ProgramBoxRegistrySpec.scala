//package co.topl.nodeView.state
//
//import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
//import co.topl.modifier.box.StateBox
//import co.topl.utils.GeneratorOps.GeneratorOps
//import io.circe.Json
//import io.circe.syntax._
//import co.topl.utils.codecs._
//
//class ProgramBoxRegistrySpec extends MockState {
//
//  var state: State = _
//
//  var pubKey: PublicKeyPropositionCurve25519 = _
//  var address: Address = _
//
//  val stateOne: Json =
//    s"""
//       |{ "a": "0" }
//     """.stripMargin.asJson
//
//  val stateTwo: Json =
//    s"""
//       |{"b": "1" }
//     """.stripMargin.asJson
//
//  var sboxOne: StateBox = _
//  var sboxTwo: StateBox = _
//
//  var newState_1: State = _
//
//  override def beforeAll(): Unit = {
//    super.beforeAll()
//
//    state = createState()
//
//    pubKey = propositionCurve25519Gen.sampleFirst()
//    address = pubKey.address
//
//    sboxOne = StateBox(address.evidence, 0L, programIdGen.sampleFirst(), stateOne)
//    sboxTwo = StateBox(address.evidence, 1L, programIdGen.sampleFirst(), stateTwo)
//  }
//
//  property("BifrostState should update programBoxRegistry with state box and rollback correctly") {
//
//    val changes_1: StateChanges = StateChanges(Seq(), Seq(sboxOne))
//    val pbr_changes_1 = Some(ProgramRegistryChanges(Map(), Map(sboxOne.value -> Seq(sboxOne.id))))
//    newState_1 = state.applyChanges(modifierIdGen.sampleFirst(), changes_1, None, pbr_changes_1).get
//
//    assert(newState_1.registryLookup(sboxOne.value).get.head == sboxOne.id)
//    assert(newState_1.getProgramBox[StateBox](sboxOne.value).get.bytes sameElements sboxOne.bytes)
//
//    val changes_2: StateChanges = StateChanges(Seq(sboxOne.id), Seq(sboxTwo))
//    val pbr_changes_2 =
//      Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map(sboxTwo.value -> Seq(sboxTwo.id))))
//    val newState_2 = newState_1.applyChanges(modifierIdGen.sampleFirst(), changes_2, None, pbr_changes_2).get
//
//    assert(newState_2.registryLookup(sboxTwo.value).get.head == sboxTwo.id)
//    assert(newState_2.getProgramBox[StateBox](sboxTwo.value).get.bytes sameElements sboxTwo.bytes)
//
//    val oldState = newState_2.rollbackTo(newState_1.version).get
//
//    assert(oldState.registryLookup(sboxOne.value).get.head == sboxOne.id)
//  }
//
//  property("BifrostState should tombstone program id in programBoxRegistry correctly") {
//
//    val changes_2: StateChanges = StateChanges(Seq(sboxOne.id), Seq())
//    val pbr_changes_2 = Some(ProgramRegistryChanges(Map(sboxOne.value -> Seq(sboxOne.id)), Map()))
//    val newState_2 = newState_1.applyChanges(modifierIdGen.sampleFirst(), changes_2, None, pbr_changes_2).get
//
//    assert(newState_2.registryLookup(sboxOne.value).isEmpty)
//  }
//}
