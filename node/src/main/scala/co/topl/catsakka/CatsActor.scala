package co.topl.catsakka

import akka.actor.ActorRef
import akka.pattern.ask
import akka.util.Timeout
import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.reflect.ClassTag

class CatsActor(val actorRef: ActorRef) extends AnyVal {

  def askEither[ReturnType: ClassTag](message: Any)(implicit
    timeout:                                   Timeout,
    ec:                                        ExecutionContext
  ): EitherT[Future, AskFailure, ReturnType] =
    EitherT(
      (actorRef ? message)
        .mapTo[ReturnType]
        .map(_.asRight)
        .recover { case e => AskException(e).asLeft }
    )
}

object CatsActor {
  implicit def asCatsActor(actorRef: ActorRef): CatsActor = new CatsActor(actorRef)
}

sealed abstract class AskFailure
case class AskException(throwable: Throwable) extends AskFailure
