package co.topl.api.program

import cats.implicits._
import co.topl.utils.catsInstances._
import co.topl.api.RPCMockState
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519}
import co.topl.modifier.ModifierId
import co.topl.modifier.box._
import co.topl.utils.GeneratorOps.GeneratorOps
import io.circe.syntax._
import org.scalatest.matchers.should

trait ProgramRPCMockState extends RPCMockState with should.Matchers {

  def directlyAddPBRStorage(version: ModifierId, boxes: Seq[ProgramBox]): Unit =
    // Manually manipulate state
//    state.directlyAddPBRStorage(version, boxes, view().state)
    ???

  lazy val (signSk, signPk) = sampleUntilNonEmpty(keyPairSetCurve25519Gen).head

  val publicKey: PublicKeyPropositionCurve25519 = propositionCurve25519Gen.sampleFirst()
  val address: Address = publicKey.address

  val fees: Map[String, Int] = Map(publicKey.show -> 500)

  val program: String =
    s"""
       |var a = 0
       |var b = 1
       |
       |add = function(x,y) {
       |  a = x + y
       |  return a
       |}
       |""".stripMargin

  val stateBox: StateBox = StateBox(address.evidence, 0L, programIdGen.sampleFirst(), Map("a" -> 0, "b" -> 1).asJson)

  val codeBox: CodeBox = CodeBox(
    address.evidence,
    1L,
    programIdGen.sampleFirst(),
    Seq("add = function(x,y) { a = x + y; return a }"),
    Map("add" -> Seq("Number", "Number"))
  )

  val executionBox: ExecutionBox =
    ExecutionBox(address.evidence, 2L, programIdGen.sampleFirst(), Seq(stateBox.value), Seq(codeBox.value))
}
