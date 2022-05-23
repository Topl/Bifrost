package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import cats.effect.kernel.Async
import co.topl.genus.algebras.ChainHeight
import co.topl.genus.flows.TrackLatestFlow
import co.topl.genus.services.blocks_query.BlockSorting
import co.topl.genus.typeclasses.implicits._
import co.topl.genus.types.BlockHeight
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.codecs._
import co.topl.utils.mongodb.models.BlockDataModel
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.{Document, MongoCollection}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}

/**
 * Represents the chain height of the blockchain currently stored in Mongo.
 */
object MongoChainHeight {

  val descendingHeightBlockSorting: Bson =
    BlockSorting(BlockSorting.SortBy.Height(BlockSorting.Height(descending = true))).toBsonSorting

  /**
   * Instantiates a new instance of [[ChainHeight]] with a connection to an underlying [[MongoCollection]] containing
   * [[Document]] values which are encoded [[BlockDataModel]] values.
   * @param blocksCollection the underlying collection to query for current chain height
   * @param materializer the materializer to use for instantiating streams
   * @tparam F the effect-ful type to use for results
   * @return a new instance of [[ChainHeight]]
   */
  def make[F[_]: Async](blocksCollection: MongoCollection[Document])(implicit
    materializer:                         Materializer,
    executionContext:                     ExecutionContext
  ): ChainHeight[F] = new Impl[F](blocksCollection)

  private class Impl[F[_]: Async](blocksCollection: MongoCollection[Document])(implicit
    materializer:                                   Materializer
  ) extends ChainHeight[F] {

    // asks the tracker for the current chain height
    val getHeight: () => Future[BlockHeight] =
      Source
        .tick(0.seconds, 10.seconds, NotUsed)
        .flatMapConcat(_ =>
          Source
            .fromPublisher(
              blocksCollection
                .find()
                .sort(descendingHeightBlockSorting)
                .limit(1)
            )
        )
        .mapConcat(document =>
          DocumentDecoder[BlockDataModel]
            .fromDocument(document)
            .map(block => BlockHeight(block.height))
            .toSeq
        )
        // track the latest value seen
        .toMat(TrackLatestFlow.create[BlockHeight].to(Sink.ignore))(Keep.right)
        .run()

    override def get: F[BlockHeight] = Async[F].fromFuture(Async[F].delay(getHeight()))
  }
}