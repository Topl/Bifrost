package co.topl.genus.flows

import akka.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import akka.stream.scaladsl.{Sink, Source}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import co.topl.genus.interpreters.MockChainHeight
import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document
import org.scalatest.flatspec.AsyncFlatSpecLike
import org.scalatest.matchers.should.Matchers
import co.topl.genus.typeclasses.implicits._

class FilterWithConfirmationDepthFlowSpec extends ScalaTestWithActorTestKit with AsyncFlatSpecLike with Matchers {

  import FilterWithConfirmationDepthFlowSpec._

  behavior of "Filter With Confirmation Depth Flow"

  it should "filter out any values with a height that is within the confirmation depth to the current height" in {
    val confirmationDepth = 10
    val currentChainHeight = 900

    val documentValue = defaultDocument
    val documentHeight = 895

    val documentToHeight: Document => Option[BlockHeight] = _ => Some(BlockHeight(documentHeight))

    val chainHeight = MockChainHeight.withHeight(BlockHeight(currentChainHeight))

    val underTest = FilterWithConfirmationDepthFlow.create[IO](confirmationDepth, documentToHeight, chainHeight)

    val result = Source.single(documentValue).via(underTest).runWith(Sink.seq)

    result.map(documents => documents.toList shouldBe Nil)
  }

  it should "not filter out any values that are in the confirmed range" in {
    val confirmationDepth = 10
    val currentChainHeight = 900

    val documentValue = defaultDocument
    val documentHeight = 500

    val documentToHeight: Document => Option[BlockHeight] = _ => Some(BlockHeight(documentHeight))

    val chainHeight = MockChainHeight.withHeight(BlockHeight(currentChainHeight))

    val underTest = FilterWithConfirmationDepthFlow.create[IO](confirmationDepth, documentToHeight, chainHeight)

    val result = Source.single(documentValue).via(underTest).runWith(Sink.seq)

    result.map(documents => documents.toList shouldBe List(documentValue))
  }
}

object FilterWithConfirmationDepthFlowSpec {
  val defaultDocument = Document("{ \"test\": true }")
}
