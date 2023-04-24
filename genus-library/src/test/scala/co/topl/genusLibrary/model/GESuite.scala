package co.topl.genusLibrary.model

import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import com.google.protobuf.ByteString

import scala.collection.immutable.ListSet

class GESuite extends munit.FunSuite {

  test("Genus Exception Message") {
    val msg = "boom!"
    val exception = GEs.InternalMessage(msg)
    assertEquals(exception.getMessage, msg, "message should be correct")
  }

  test("Genus Exception with cause") {
    val msg = "boom!"
    val cause = new RuntimeException()
    val exception = GEs.InternalMessageCause(msg, cause)
    assertEquals(exception.getMessage, msg, "message should be correct")
    assert(cause eq exception.getCause, "Cause should be correct")
  }

  test("Genus Exception Internal") {
    interceptMessage[GE]("boom!") {
      throw GEs.Internal(new RuntimeException("boom!"))
    }
  }

  test("Genus Exception UnImplemented") {
    interceptMessage[GE]("An implementation is missing") {
      throw GEs.UnImplemented
    }
  }

  test("Genus Exception NotFound") {
    interceptMessage[GE]("boom!") {
      throw GEs.NotFound("boom!")
    }
  }

  test("Genus Exception HeaderNotFound") {
    val blockId = BlockId.of(value = ByteString.copyFrom(Array.fill[Byte](32)(0)))
    val expected = "Block header wasn't found. BlockId=[b_11111111111111111111111111111111]"
    interceptMessage[GE](expected) {
      throw GEs.HeaderNotFound(blockId)
    }
  }

  test("Genus Exception BodyNotFound") {
    val blockId = BlockId.of(value = ByteString.copyFrom(Array.fill[Byte](32)(0)))
    val expected = "Block body wasn't found. BlockId=[b_11111111111111111111111111111111]"
    interceptMessage[GE](expected) {
      throw GEs.BodyNotFound(blockId)
    }
  }

  test("Genus Exception TransactionsNotFound") {
    val ioTxId_1 =
      TransactionId(ByteString.copyFrom(Array.fill[Byte](32)(1)))

    val ioTxId_2 =
      TransactionId(ByteString.copyFrom(Array.fill[Byte](32)(2)))

    val ioTx32s = ListSet.from(ListSet(ioTxId_1, ioTxId_2))

    val expected =
      "Transactions weren't found. Transactions=[ListSet(t_4vJ9JU1bJJE96FWSJKvHsmmFADCg4gpZQff4P3bkLKi, t_8qbHbw2BbbTHBW1sbeqakYXVKRQM8Ne7pLK7m6CVfeR)]"

    interceptMessage[GE](expected) {
      throw GEs.TransactionsNotFound(ioTx32s)
    }
  }
}
