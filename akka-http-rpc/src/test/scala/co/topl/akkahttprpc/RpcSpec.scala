package co.topl.akkahttprpc

import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.{EitherT, NonEmptyMap}
import cats.implicits._
import co.topl.akkahttprpc.JsonFailureSupport.throwableEncoder
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class RpcSpec extends AnyFlatSpecLike with ScalatestRouteTest with EitherValues with Matchers with ScalaFutures {

  behavior of "Rpc"

  implicit val decodeParams: Decoder[TestMethod1Params] =
    deriveDecoder[TestMethod1Params]

  implicit val encodeParams: Encoder[TestMethod1Params] =
    deriveEncoder[TestMethod1Params]

  implicit val encodeTestMethod1Error: Encoder[TestMethod1Error] =
    Encoder.forProduct1[TestMethod1Error, Throwable]("throwable")(e => e.throwable)

  implicit val encodeTestMethod1Success: Encoder[TestMethod1Success] =
    deriveEncoder[TestMethod1Success]

  implicit val errorToRpcError: RpcErrorEncoder[TestMethod1Error] =
    e => CustomError.fromThrowable(32034, "Test Exception", e.throwable)

  it should "create akka-http Routes using RPC handlers" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethod1Params, TestMethod1Error, TestMethod1Success](params =>
        EitherT.pure[Future, TestMethod1Error](TestMethod1Success(params.userId.length))
      )

    val underTest = Rpc.route(
      NonEmptyMap.of(
        "test_method1" -> method1Handler
      )
    )

    Post(
      "/",
      Map(
        "id"      -> "1".asJson,
        "jsonrpc" -> "2.0".asJson,
        "method"  -> "test_method1".asJson,
        "params"  -> TestMethod1Params("abcdef").asJson
      ).asJson
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value shouldBe "1"
      c.downField("result").downField("value").as[Int].value shouldBe 6
    }
  }

  it should "return a ParseError when non-JSON is provided" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethod1Params, TestMethod1Error, TestMethod1Success](params =>
        EitherT.pure[Future, TestMethod1Error](TestMethod1Success(params.userId.length))
      )

    val underTest = Rpc.route(
      NonEmptyMap.of(
        "test_method1" -> method1Handler
      )
    )

    Post(
      "/",
      "<xml></xml>"
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value
      c.downField("error").downField("code").as[Int].value shouldBe -32700
      c.downField("error").downField("message").as[String].value shouldBe "Invalid JSON"
    }
  }

  it should "return an InvalidRequestError when JSON is provided but is not valid RPC format" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethod1Params, TestMethod1Error, TestMethod1Success](params =>
        EitherT.pure[Future, TestMethod1Error](TestMethod1Success(params.userId.length))
      )

    val underTest = Rpc.route(
      NonEmptyMap.of(
        "test_method1" -> method1Handler
      )
    )

    Post(
      "/",
      """{"foo": "bar"}"""
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value
      c.downField("error").downField("code").as[Int].value shouldBe -32600
      c.downField("error").downField("message").as[String].value shouldBe "Invalid RPC Request Format"
    }
  }

  case class TestMethod1Params(userId: String)
  case class TestMethod1Error(throwable: Throwable)

  case class TestMethod1Success(value: Int)

}
