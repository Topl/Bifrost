package example

//Import relevant akka libraries
import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import example.requests.Requests
import org.scalatest.{AsyncFlatSpec, Matchers}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256

import scala.concurrent.Future

class RequestSpec extends AsyncFlatSpec with Matchers {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import actorSystem.dispatcher

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests

  //Encode two keys after generating them from entropy
    //Keys are NOT the same, per each instance of entropic input
  val keyOne = Base58.encode(PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2.pubKeyBytes)
  val keyTwo = Base58.encode(PrivateKey25519Companion.generateKeys(Blake2b256(java.util.UUID.randomUUID.toString))._2.pubKeyBytes)

  val amount = 10

  it should "receive a successful response from Bifrost" in {
    val createAssetRequest = requests.transaction("createAssetsPrototype", keyOne, keyTwo, amount)

    val sendTx = requests.httpPOST(createAssetRequest)

    val response: Future[HttpResponse] = http.singleRequest(sendTx)

    response.map { res => {
      res.status shouldBe StatusCodes.OK
//      res.entity.dataBytes
      // need to change it to something other than a foreach to return ByteString -> later to be parsed as JSON
      // transforming ByteString to JSON -> should be its own fxn because used a lot (data being received from requests)
      }
    }

    /* to check the values here???
    viewAsync().map { view =>
      val issuer = PublicKey25519Proposition(
        Base58.decode((params \\ "issuer").head.asString.get).get
      )
      val recipient: PublicKey25519Proposition = PublicKey25519Proposition(
        Base58.decode((params \\ "recipient").head.asString.get).get
      )
      val amount: Long = (params \\ "amount").head.asNumber.get.toLong.get
      val assetCode: String =
        (params \\ "assetCode").head.asString.getOrElse("")
      val fee: Long =
        (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None          => ""
      }
     */

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

//  "sign transaction" {
//
//  }

//  "broadcast transaction" {
//
//  }
}
