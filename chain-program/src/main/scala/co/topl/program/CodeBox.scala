//package co.topl.modifier.box
//
//import co.topl.attestation.Evidence
//import co.topl.modifier.box.Box.BoxType
//import co.topl.utils.{Identifiable, Identifier}
//
//case class CodeBox(
//  override val evidence: Evidence,
//  override val nonce:    Box.Nonce,
//  override val value:    ProgramId,
//  code:                  Seq[String], // List of strings of JS functions
//  interface:             Map[String, Seq[String]]
//) extends ProgramBox(evidence, nonce, value)
//
//object CodeBox {
//  val typePrefix: BoxType = 13: Byte
//  val typeString: String = "CodeBox"
//
//  implicit val identifier: Identifiable[CodeBox] = Identifiable.instance { () =>
//    Identifier(typeString, typePrefix)
//  }
//}
