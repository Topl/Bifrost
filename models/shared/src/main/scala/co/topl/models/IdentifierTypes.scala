package co.topl.models

object IdentifierTypes {

  object Block {
    val V1: TypePrefix = 1: Byte
    val HeaderV2: TypePrefix = 4: Byte
    val BodyV2: TypePrefix = 4: Byte
  }

  val Transaction: TypePrefix = 3: Byte

  object Box {
    val Arbit: TypePrefix = 1: Byte
    val Poly: TypePrefix = 2: Byte
    val Asset: TypePrefix = 3: Byte
    val Registration: TypePrefix = 4: Byte
    val Taktikos: TypePrefix = 5: Byte
  }

  val RatioEvidence: TypePrefix = 6: Byte
}
