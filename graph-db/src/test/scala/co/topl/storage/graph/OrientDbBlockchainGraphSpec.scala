package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.testkit.TestKit
import cats.data.NonEmptyChain
import org.scalatest.{BeforeAndAfterAll, EitherValues, OptionValues}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Paths}

class OrientDbBlockchainGraphSpec
    extends TestKit(ActorSystem("OrientDbBlockchainGraphSpec"))
    with AnyFlatSpecLike
    with BeforeAndAfterAll
    with ScalaFutures
    with EitherValues
    with OptionValues
    with Matchers {

  behavior of "OrientDbBlockchainGraph"

  private var underTest: BlockchainOpsProvider = _

  it should "Add blocks" in {
    val t = underTest
    import t._
    val blockHeader = BlockHeader(
      id = "foo",
      timestamp = 1,
      publicKey = "topl",
      signature = "topl",
      height = 1,
      difficulty = 1,
      txRoot = "bar",
      bloomFilter = "baz",
      version = 1
    )

    NonEmptyChain(CreateBlockHeader(blockHeader)).run().value.futureValue.value

    "foo".blockHeader.value.futureValue.value shouldBe blockHeader
  }

  override def beforeAll(): Unit = {
    super.beforeAll()

    underTest = new OrientDbBlockchainGraph(Paths.get("./target/testDb"))
//    underTest = new OrientDbBlockchainGraph(Files.createTempDirectory("OrientDbBlockchainGraph"))
  }
}
