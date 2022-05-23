package co.topl.genus.flows

import cats.implicits._
import akka.NotUsed
import akka.stream.scaladsl.{Flow, Source}
import cats.~>
import co.topl.genus.algebras.ChainHeight
import org.mongodb.scala.bson.collection.immutable.Document
import co.topl.genus.ops.implicits._
import co.topl.genus.types.BlockHeight

import scala.concurrent.Future

object FilterWithConfirmationDepthFlow {

  /**
   * Creates a flow which filters out any values outside of the confirmed range defined by the parameters.
   * @param confirmationDepth the number of blocks below the chain height that a value must exist in to be confirmed
   * @param documentToHeightOpt a function which retrieves the block height from a mongo document
   * @param chainHeight an instance of the [[ChainHeight]] algebra
   * @tparam F a context with an instance of ~> to Future
   * @return an instance of a [[Flow]] which takes a [[Document]] from upstream and emits a [[Document]] downstream.
   */
  def create[F[_]: *[_] ~> Future](
    confirmationDepth:   Int,
    documentToHeightOpt: Document => Option[BlockHeight],
    chainHeight:         ChainHeight[F]
  ): Flow[Document, Document, NotUsed] =
    Flow[Document].flatMapConcat(document =>
      Source
        .asyncF(chainHeight.get)
        .mapConcat(height =>
          documentToHeightOpt(document)
            .flatMap(documentHeight =>
              if (documentHeight.value <= height.value - confirmationDepth) document.some
              else None
            )
            .toList
        )
    )
}
