package co.topl.genusLibrary.model

class GenusExceptionSuite extends munit.FunSuite {
  val msg = "eieio"
  val cause = new RuntimeException()

  test("GenusExceptionNoCause") {
    val exception = GenusExceptions.Message(msg)
    assertEquals(exception.getMessage, msg, "message should be correct")
  }

  test("GenusExceptionWithCause") {
    val exception = GenusExceptions.MessageWithCause(msg, cause)
    assertEquals(exception.getMessage, msg, "message should be correct")
    assert(cause eq exception.getCause, "Cause should be correct")
  }
}
