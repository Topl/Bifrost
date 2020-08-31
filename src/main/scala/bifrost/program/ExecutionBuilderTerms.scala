package bifrost.program

import akka.actor.ActorSystem
import akka.http.scaladsl.coding.{Coders, Gzip}
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base64

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class ExecutionBuilderTerms(terms: String){
  implicit val actorsystem = ActorSystem()
  implicit val materializer = ActorMaterializer()

  require(terms.length < 16*1024)
  lazy val json: Json = if(terms.length > 1024) {
    s"gzip:${Base64.encode(Await.result(Coders.Gzip.encodeAsync(ByteString(terms.getBytes)), 10 seconds).toArray[Byte])}".asJson
  } else {
    terms.asJson
  }

  override def toString: String = s"ExecutionBuilderTerms(${json.toString})"
}

object ExecutionBuilderTerms {

  implicit val system = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  def decodeGzip(zipped: String): Future[ByteString] = {
    Coders.Gzip.decode(ByteString(Base64.decode(zipped)))
  }

  implicit val decodeTerms: Decoder[ExecutionBuilderTerms] = (c: HCursor) => for {
    terms <- c.as[String]
  } yield {
    import scala.concurrent.ExecutionContext.Implicits.global
    if(terms.startsWith("gzip:")) {
      Await.result({
        for {
          decodedTerms <- decodeGzip(terms.substring("gzip:".length))
        } yield ExecutionBuilderTerms(new String(decodedTerms.toArray[Byte]))
      }, Duration.Inf)
    } else {
      ExecutionBuilderTerms(terms)
    }
  }
}