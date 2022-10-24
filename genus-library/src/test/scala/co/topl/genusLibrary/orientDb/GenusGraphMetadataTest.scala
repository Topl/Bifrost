package co.topl.genusLibrary.orientDb

import scala.util.Random

class GenusGraphMetadataTest extends munit.FunSuite {
  import GenusGraphMetadata._

  val TypedBytesLength = 33

  test("typedBytesSerialization") {
    val byteArray = Random.nextBytes(TypedBytesLength)
    assertEquals(typedBytesTupleToByteArray(byteArrayToTypedBytesTuple(byteArray)).toSeq, byteArray.toSeq,
      "Round trip serialization of TypedBytes")
  }
}
