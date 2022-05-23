package co.topl.genus.interpreters

import cats.Id
import cats.implicits._
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class BatchedMongoSubscriptionSpec extends AnyFlatSpec with Matchers {
  import BatchedMongoSubscriptionSpec._

  behavior of "Batched Mongo Subscription whenConfirmed"

  it should "return OptionT-None when value has not been confirmed" in {
    val confirmationDepth = 15
    val currentChainHeight = 1000

    val document = defaultDocument
    val documentToHeight = (_: Document) => BlockHeight(999).some

    val chainHeight = MockChainHeight.withHeight[Id](BlockHeight(currentChainHeight))

    val underTest = BatchedMongoSubscription.whenConfirmed[Id](confirmationDepth, documentToHeight, chainHeight) _

    val result = underTest(document)

    result.value shouldBe None
  }

  it should "return OptionT-Some with document when value has been confirmed" in {
    val confirmationDepth = 15
    val currentChainHeight = 1000

    val document = defaultDocument
    val documentToHeight = (_: Document) => BlockHeight(500).some

    val chainHeight = MockChainHeight.withHeight[Id](BlockHeight(currentChainHeight))

    val underTest = BatchedMongoSubscription.whenConfirmed[Id](confirmationDepth, documentToHeight, chainHeight) _

    val result = underTest(document)

    result.value shouldBe Some(document)
  }

  it should "return OptionT-None when height can not be retrieved from the document" in {
    val confirmationDepth = 15
    val currentChainHeight = 1000

    val document = defaultDocument
    val documentToHeight = (_: Document) => none[BlockHeight]

    val chainHeight = MockChainHeight.withHeight[Id](BlockHeight(currentChainHeight))

    val underTest = BatchedMongoSubscription.whenConfirmed[Id](confirmationDepth, documentToHeight, chainHeight) _

    val result = underTest(document)

    result.value shouldBe None
  }
}

object BatchedMongoSubscriptionSpec {
  val defaultDocument = Document("{ \"test\": true }")
}
