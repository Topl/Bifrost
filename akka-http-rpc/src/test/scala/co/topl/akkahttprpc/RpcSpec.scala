package co.topl.akkahttprpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.JsonFailureSupport.ThrowableSupport.Verbose._
import co.topl.akkahttprpc.RpcDirectives._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.optics.JsonPath._
import io.circe.syntax._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside, OptionValues}

import scala.concurrent.Future

class RpcSpec
    extends AnyFlatSpecLike
    with ScalatestRouteTest
    with EitherValues
    with Matchers
    with ScalaFutures
    with OptionValues
    with Inside {

  behavior of "Rpc"

  import RpcSpec._

  it should "successfully handle an RPC call" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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

      root.id.string.getOption(json).value shouldBe "1"
      root.result.value.int.getOption(json).value shouldBe 6
    }
  }

  it should "successfully handle an RPC call with array-based parameters" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
      )

    Post(
      "/",
      Map(
        "id"      -> "1".asJson,
        "jsonrpc" -> "2.0".asJson,
        "method"  -> "test_method1".asJson,
        "params"  -> List(TestMethodParams("abcdef").asJson).asJson
      ).asJson
    ) ~> underTest ~> check {
      val json = responseAs[Json]

      root.id.string.getOption(json).value shouldBe "1"
      root.result.value.int.getOption(json).value shouldBe 6
    }
  }

  it should "return a ParseError when non-JSON is provided" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
      )

    Post(
      "/",
      "<xml></xml>"
    ) ~> underTest ~> check {
      rejection shouldBe RpcErrorRejection(ParseError)
    }
  }

  it should "return an InvalidRequestError when JSON is provided but is not valid RPC format" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
      )

    Post(
      "/",
      Map("foo" -> "bar").asJson
    ) ~> underTest ~> check {
      inside(rejection) { case RpcErrorRejection(e) =>
        e shouldBe a[InvalidRequestError]
      }
    }
  }

  it should "return a MethodNotFoundError when valid RPC format is provided but the method is not known" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
      inside(rejection) { case RpcErrorRejection(e) =>
        e shouldBe MethodNotFoundError("unknown_method")
      }
    }
  }

  it should "choose the correct handler" in {

    val underTest =
      rpcRoute(
        "test_method2",
        (_: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(10))
      ) ~ rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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

      root.id.string.getOption(json).value shouldBe "1"
      root.result.value.int.getOption(json).value shouldBe 6
    }
  }

  it should "return a InvalidParametersError when valid RPC format is provided but the method is not known" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (params: TestMethodParams) => EitherT.pure[Future, TestMethodError](TestMethodSuccess(params.userId.length))
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
      inside(rejection) { case RpcErrorRejection(e) =>
        e shouldBe a[InvalidParametersError]
      }
    }
  }

  it should "return a custom error when an RPC results in an unhandled exception" in {
    val underTest =
      rpcRoute(
        "test_method1",
        (_: TestMethodParams) =>
          EitherT[Future, TestMethodError, TestMethodSuccess](Future.failed(new Exception("Heck")))
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
      inside(rejection) { case RpcErrorRejection(e) =>
        e shouldBe a[CustomError[_]]
      }
    }
  }

  it should "return a custom error when an RPC results in a known error" in {

    val underTest =
      rpcRoute(
        "test_method1",
        (_: TestMethodParams) =>
          EitherT.fromEither[Future](TestMethodError(new Exception("Heck")).asLeft[TestMethodSuccess])
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
      inside(rejection) { case RpcErrorRejection(e) =>
        e shouldBe a[CustomError[_]]
      }
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

  implicit val errorToRpcError: ToRpcError[TestMethodError] =
    e => CustomError.fromThrowable(-32034, "Test Exception", e.throwable)

}

case class TestMethodParams(userId: String)
case class TestMethodError(throwable: Throwable)

case class TestMethodSuccess(value: Int)
