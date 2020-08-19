package example

//Import relevant akka libraries
import akka.actor.ActorSystem
import akka.http.scaladsl.{Http, HttpExt}
import akka.http.scaladsl.model.{HttpResponse, StatusCodes}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Source
import akka.util.ByteString
import example.requests.Requests
import io.circe.{Json, ParsingFailure, parser}
import org.scalatest.{AsyncFlatSpec, Matchers}
import scorex.crypto.encode.Base58
import scorex.crypto.hash.Blake2b256
import io.circe.syntax._

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.reflect.io.Path
import scala.util.Try

class RequestSpec extends AsyncFlatSpec with Matchers {

  implicit val actorSystem: ActorSystem = ActorSystem()
  implicit val materializer: ActorMaterializer = ActorMaterializer()
  import actorSystem.dispatcher

  val http: HttpExt = Http(actorSystem)

  val requests = new Requests

  val seed1 = Blake2b256(java.util.UUID.randomUUID.toString)
  val seed2 = Blake2b256(java.util.UUID.randomUUID.toString)
  val (sk1, pk1) = PrivateKey25519Companion.generateKeys(seed1)
  val (sk2, pk2) = PrivateKey25519Companion.generateKeys(seed2)

  val keyFileDir = "keyfiles/keyManagerTest"
  val path: Path = Path(keyFileDir)
  Try(path.deleteRecursively())
  Try(path.createDirectory())
  val password = "pass"

  val keyFile = KeyFile(password, seed1, keyFileDir)
  val keyManager = KeyManager(Set(), keyFileDir)
  keyManager.unlockKeyFile(Base58.encode(sk1.publicKeyBytes), password)

  val amount = 10

  var transaction = Json.Null

  it should "receive a successful response from Bifrost upon creating asset" in {
    val createAssetRequest = requests.transaction("createAssetsPrototype", Base58.encode(pk1.pubKeyBytes), Base58.encode(pk2.pubKeyBytes), amount)
    transaction = requests.sendRequest(createAssetRequest)
    println(transaction)
    assert(transaction != null)
  }

  it should "receive JSON from sign transaction" in {
    val issuer: List[String] = List(Base58.encode(pk1.pubKeyBytes))
    val JSON = requests.signTx(transaction, keyManager, issuer)
    println(JSON)
    val sigs = (JSON \\ "signatures").head.asObject.get
    issuer.foreach(key => assert(sigs.contains(key)))

    assert((JSON \\ "signatures").head != null)
  }

//  "broadcast transaction" {
//
//  }
}
