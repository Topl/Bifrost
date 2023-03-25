package co.topl.genusLibrary.model

class GRESuite extends munit.FunSuite {
  val msg = "eieio"
  val cause = new RuntimeException()

  test("GenusExceptionNoCause") {
    val exception = GREs.Message(msg)
    assertEquals(exception.getMessage, msg, "message should be correct")
  }

  test("GenusExceptionWithCause") {
    val exception = GREs.MessageCause(msg, cause)
    assertEquals(exception.getMessage, msg, "message should be correct")
    assert(cause eq exception.getCause, "Cause should be correct")
  }
}
