package co.topl.genus.interpreters

import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Keep, Sink, Source}
import cats.Applicative
import cats.implicits._
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
  def make[F[_]: Applicative](blocksCollection: MongoCollection[Document])(implicit
    materializer:                               Materializer
  ): ChainHeight[F] = new Impl[F](blocksCollection)

  private class Impl[F[_]: Applicative](blocksCollection: MongoCollection[Document])(implicit
    materializer:                                         Materializer
  ) extends ChainHeight[F] {

    val defaultValue: BlockHeight = BlockHeight(0)

    // get height will ask the tracker for the current chain height
    val getHeight: () => BlockHeight =
      Source
        .tick(0.seconds, 10.seconds, NotUsed)
        .flatMapConcat(_ =>
          // get the current largest value
          Source
            .fromPublisher(
              blocksCollection.find().sort(descendingHeightBlockSorting)
            )
            .take(1)
        )
        // decode the documents into blocks and get the height
        .mapConcat(document =>
          DocumentDecoder[BlockDataModel].fromDocument(document).map(block => BlockHeight(block.height)).toSeq
        )
        // track the latest value seen
        .toMat(TrackLatestFlow.create(defaultValue).to(Sink.ignore))(Keep.right)
        .run()

    override def get: F[BlockHeight] = getHeight().pure[F]
  }
}
