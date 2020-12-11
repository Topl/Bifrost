package example

import akka.actor.{ActorRef, ActorSystem}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.model.headers.RawHeader
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.util.ByteString
import crypto.AddressEncoder.NetworkPrefix
import crypto.{Address, KeyfileCurve25519, PrivateKeyCurve25519}
import http.{HttpService, KeyManagementApi}
import io.circe.Json
import io.circe.parser.parse
import keymanager.{KeyManagerRef, Keys}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import requests.ApiRoute

import scala.util.{Failure, Success}

class KeyManagementRPCSpec extends AsyncFlatSpec
  with Matchers
  with ScalatestRouteTest
  with GjallarhornGenerators {

  //Make sure running bifrost in local network!
  implicit val networkPrefix: NetworkPrefix = 48.toByte

  override def createActorSystem(): ActorSystem = ActorSystem("keyManagementTest", keysConfig)

  val keyFileDir = "keyfiles/keyManagerTest"
  val keyManager: Keys[PrivateKeyCurve25519, KeyfileCurve25519] =
    Keys[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)
  val keyManagerRef: ActorRef = KeyManagerRef("keyManager", keyManager)

  val apiRoute: ApiRoute = KeyManagementApi(keyManagementSettings, keyManagerRef)
  val route: Route = HttpService(Seq(apiRoute), keyManagementSettings.rpcApi).compositeRoute

  val privateKeys: Set[PrivateKeyCurve25519] = keyManager.generateNewKeyPairs(2, Some("test")) match {
    case Success(secrets) => secrets
    case Failure(ex) => throw new Error (s"Unable to generate new keys: $ex")
  }

  val addresses: Set[Address] = privateKeys.map(sk => sk.publicImage.address)
  val (pk1, sk1) = (addresses.head, privateKeys.head)
  val (pk2, sk2) = (addresses.tail.head, privateKeys.tail.head)


  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    ).withHeaders(RawHeader("x-api-key", "test_key"))
  }

  it should "successfully get open keyfiles" in {
    val openKeyfilesRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_listOpenKeyfiles",
         |   "params": [{}]
         |}
         """.stripMargin)

    httpPOST(openKeyfilesRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          val openKeys: Set[String] = (res \\ "result").head.asArray.get.map(k => k.asString.get).toSet
          openKeys.contains(pk1.toString) shouldBe true
      }
    }
  }

  var pubKeyAddr: String = pk1.toString

  it should "successfully generate a keyfile" in {
    val generateKeyfileRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_generateKeyfile",
         |   "params": [{
         |      "password": "foo"
         |   }]
         |}
         """.stripMargin)

    httpPOST(generateKeyfileRequest) ~> route ~> check {
      parse(responseAs[String]) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          val result: Json = (res \\ "result").head
          pubKeyAddr = (result \\ "address").head.asString.get
          (res \\ "error").isEmpty shouldBe true
          result.asObject.isDefined shouldBe true
      }
    }
  }

  val seedPhrase: String = "stand earth guess employ goose aisle great next embark weapon wonder aisle " +
    "monitor surface omit guilt model rule"

  it should "successfully import a keyfile through mnemonic phrase" in {
    val importKeyfileRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_importKeyfile",
         |   "params": [{
         |      "password": "password",
         |      "seedPhrase": "$seedPhrase",
         |      "seedPhraseLang": "en"
         |   }]
         |}
         """.stripMargin)

    httpPOST(importKeyfileRequest) ~> route ~> check {
      parse(responseAs[String]) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          val result: Json = (res \\ "result").head
          (res \\ "error").isEmpty shouldBe true
          result.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully lock a Keyfile" in {
    val lockKeyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_lockKeyfile",
         |   "params": [{
         |      "publicKey": "$pubKeyAddr",
         |      "password": "foo"
         |   }]
         |}
           """.stripMargin)

    httpPOST(lockKeyRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully unlock a Keyfile" in {
    val unlockKeyRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_unlockKeyfile",
         |   "params": [{
         |      "publicKey": "$pubKeyAddr",
         |      "password": "foo"
         |   }]
         |}
         """.stripMargin)

    httpPOST(unlockKeyRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          (res \\ "result").head.asObject.isDefined shouldBe true
      }
    }
  }

  it should "successfully get mnemonic phrase" in {
    val mnemonicPhraseRequest = ByteString(
      s"""
         |{
         |   "jsonrpc": "2.0",
         |   "id": "2",
         |   "method": "wallet_generateMnemonic",
         |   "params": [{
         |      "language": "en"
         |   }]
         |}
         """.stripMargin)

    httpPOST(mnemonicPhraseRequest) ~> route ~> check {
      val responseString = responseAs[String].replace("\\", "")
      parse(responseString.replace("\"{", "{").replace("}\"", "}")) match {
        case Left(f) => throw f
        case Right(res: Json) =>
          (res \\ "error").isEmpty shouldBe true
          val phrase = ((res \\ "result").head \\ "mnemonicPhrase").head
          assert(phrase != null)
      }
    }
  }

}
