package co.topl.genus.flows

import akka.NotUsed
import akka.stream.scaladsl.{Flow, Source}
import cats.data.OptionT
import cats.~>
import co.topl.genus.ops.implicits._

import scala.concurrent.Future

object FutureOptionTFilterFlow {

  def create[F[_]: *[_] ~> Future, A, B](test: A => OptionT[F, B]): Flow[A, B, NotUsed] =
    Flow[A]
      .flatMapConcat(value =>
        Source
          .asyncF(
            test(value).value
          )
          .mapConcat(valueOpt => valueOpt.fold[List[B]](Nil)(valueSome => List(valueSome)))
      )
}
