package co.topl.genus.programs

import akka.{Done, NotUsed}
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import cats.Id
import co.topl.genus.algebras.SubscriptionAlg
import co.topl.modifier.block.Block
import co.topl.utils.mongodb.models.BlockDataModel
import org.mongodb.scala.bson.conversions.Bson
import org.mongodb.scala.model.Filters
import org.mongodb.scala.model.changestream.ChangeStreamDocument

import scala.concurrent.Future

object TestBlockStream {

  object Eval {

    def make(subscription: SubscriptionAlg[Id, Source[*, NotUsed], Bson, ChangeStreamDocument[BlockDataModel]])(implicit
      materializer:        Materializer
    ): Future[Done] =
      subscription.subscribe(Filters.empty()).map(_.getFullDocument).runWith(Sink.foreach(println))
  }
}
