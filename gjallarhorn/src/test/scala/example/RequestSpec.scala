package example

import akka.actor.Status.{Failure, Success}
import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import example.requests.Requests
import org.scalatest.{Matchers, WordSpec}
import scorex.crypto.hash.Blake2b256

import scala.concurrent.Future

class RequestSpec extends WordSpec with Matchers {

  implicit val actorSystem = ActorSystem()
  implicit val materializer = ActorMaterializer()
  import actorSystem.dispatcher


  val http = Http(actorSystem)

  val requests = new Requests

  val keyOne = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ" //PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2
  val keyTwo = "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb" //PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2

  val amount = 10

  "Creating a new asset transaction" should {
    "receive a successful response from Bifrost" in {
      val createAssetRequest = requests.transaction("createAssetsPrototype", keyOne.toString, keyTwo.toString, amount)

      val sendTx = requests.httpPOST(createAssetRequest)

      val response: Future[HttpResponse] = http.singleRequest(sendTx)

      response.map {
        case response@HttpResponse(StatusCodes.OK, headers, entity, _) =>
          entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
            println(body.utf8String)
          }
        case response @ HttpResponse(code, _, _, _) =>
          println(code)
      }
    }
  }
}
