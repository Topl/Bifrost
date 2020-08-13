package example

import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import example.requests.Requests
import org.scalatest.{AsyncFlatSpec, Matchers}

import scala.concurrent.Future

class RequestSpec extends AsyncFlatSpec with Matchers {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import actorSystem.dispatcher

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests

  val keyOne = "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ" //PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2
  val keyTwo = "A9vRt6hw7w4c7b4qEkQHYptpqBGpKM5MGoXyrkGCbrfb" //PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2

  val amount = 10

  it should "receive a successful response from Bifrost" in {
    val createAssetRequest = requests.transaction("createAssetsPrototype", keyOne, keyTwo, amount)

    val sendTx = requests.httpPOST(createAssetRequest)

    val response: Future[HttpResponse] = http.singleRequest(sendTx)

    response.map { res => res.status shouldBe StatusCodes.OK }

    /*response.map {
      case response@HttpResponse(StatusCodes.OK, headers, entity, _) =>
        entity.dataBytes.runFold(ByteString(""))(_ ++ _).foreach { body =>
          println(body.utf8String)
        }
      case response @ HttpResponse(code, _, _, _) =>
        println(code)
    }
     */
  }

  "sign transaction" {

  }

  "broadcast transaction" {

  }
}
