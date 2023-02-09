package co.topl.genusLibrary

class GenusExceptionTest extends munit.FunSuite {
  val msg = "eieio"
  val cause = new RuntimeException()

  test("GenusExceptionNoCause") {
    val exception = GenusException(msg)
    assertEquals(exception.getMessage, msg, "message should be correct")
  }

  test("GenusExceptionWithCause") {
    val exception = GenusException(msg, cause)
    assertEquals(exception.getMessage, msg, "message should be correct")
    assert(cause eq exception.getCause, "Cause should be correct")
  }
}
