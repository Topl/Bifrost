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

  import RpcSpec._

  it should "create akka-http Routes using RPC handlers" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethodParams, TestMethodError, TestMethodSuccess](params =>
        EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
        "params"  -> TestMethodParams("abcdef").asJson
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
      RpcHandler[TestMethodParams, TestMethodError, TestMethodSuccess](params =>
        EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
      RpcHandler[TestMethodParams, TestMethodError, TestMethodSuccess](params =>
        EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
      )

    val underTest = Rpc.route(
      NonEmptyMap.of(
        "test_method1" -> method1Handler
      )
    )

    Post(
      "/",
      Map("foo" -> "bar").asJson
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value
      c.downField("error").downField("code").as[Int].value shouldBe -32600
      c.downField("error").downField("message").as[String].value shouldBe "Invalid RPC Request Format"
    }
  }

  it should "return a MethodNotFoundError when valid RPC format is provided but the method is not known" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethodParams, TestMethodError, TestMethodSuccess](params =>
        EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
        "method"  -> "unknown_method".asJson,
        "params"  -> Json.obj()
      ).asJson
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value
      c.downField("error").downField("code").as[Int].value shouldBe -32601
      c.downField("error").downField("message").as[String].value shouldBe "RPC Method Not Found"
    }
  }

  it should "return a InvalidParametersError when valid RPC format is provided but the method is not known" in {
    val method1Handler: RpcHandler[_, _, _] =
      RpcHandler[TestMethodParams, TestMethodError, TestMethodSuccess](params =>
        EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
        "params"  -> Map("foo" -> "bar").asJson
      ).asJson
    ) ~> underTest ~> check {
      val json = responseAs[Json]
      val c = json.hcursor
      c.downField("id").as[String].value
      c.downField("error").downField("code").as[Int].value shouldBe -32602
      c.downField("error").downField("message").as[String].value shouldBe "Invalid method parameter(s)"
      c.downField("error")
        .downField("data")
        .downField("errors")
        .downArray
        .downField("path")
        .as[List[String]]
        .value shouldBe List(
        "--\\(userId)"
      )
    }
  }

}

object RpcSpec {

  implicit val decodeParams: Decoder[TestMethodParams] =
    deriveDecoder[TestMethodParams]

  implicit val encodeParams: Encoder[TestMethodParams] =
    deriveEncoder[TestMethodParams]

  implicit val encodeTestMethod1Error: Encoder[TestMethodError] =
    Encoder.forProduct1[TestMethodError, Throwable]("throwable")(e => e.throwable)

  implicit val encodeTestMethod1Success: Encoder[TestMethodSuccess] =
    deriveEncoder[TestMethodSuccess]

  implicit val errorToRpcError: RpcErrorEncoder[TestMethodError] =
    e => CustomError.fromThrowable(32034, "Test Exception", e.throwable)

}

case class TestMethodParams(userId: String)
case class TestMethodError(throwable: Throwable)

case class TestMethodSuccess(value: Int)
