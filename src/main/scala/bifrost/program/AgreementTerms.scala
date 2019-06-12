package bifrost.program

import akka.actor.ActorSystem
import akka.http.scaladsl.coding.Gzip
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._
import scorex.crypto.encode.Base64

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

case class AgreementTerms(terms: String){

  require(terms.length < 16*1024)
  lazy val json: Json = if(terms.length > 1024) {
    s"gzip:${Base64.encode(Gzip.encode(ByteString(terms.getBytes)).toArray[Byte])}".asJson
  } else {
    terms.asJson
  }

  override def toString: String = s"AgreementTerms(${json.toString})"
}

object AgreementTerms {

  implicit val system = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  def decodeGzip(zipped: String): Future[ByteString] = {
    Gzip.decode(ByteString(Base64.decode(zipped)))
  }

  implicit val decodeTerms: Decoder[AgreementTerms] = (c: HCursor) => for {
    terms <- c.as[String]
  } yield {

    if(terms.startsWith("gzip:")) {
      Await.result({
        import scala.concurrent.ExecutionContext.Implicits.global
        for {
          decodedTerms <- decodeGzip(terms.substring("gzip:".length))
        } yield AgreementTerms(new String(decodedTerms.toArray[Byte]))
      }, Duration.Inf)
    } else {
      AgreementTerms(terms)
    }
  }


  // def validate(terms: AgreementTerms): Try[Unit] = Try {
  //   require(terms.pledge > 0)
  //   require(terms.xrate > 0)
  // }
}