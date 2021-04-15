package co.topl.akkahttprpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.ThrowableSupport.Verbose.verboseThrowableCodec
import co.topl.akkahttprpc.implicits.server._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.optics.JsonPath._
import io.circe.syntax._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, Inside, OptionValues}
import RpcEncoders._
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
    val underTest = normalRoute

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
    val underTest = normalRoute

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
    val underTest = normalRoute

    Post(
      "/",
      "<xml></xml>"
    ) ~> underTest ~> check {
      rejection shouldBe RpcErrorRejection(ParseError)
    }
  }

  it should "return an InvalidRequestError when JSON is provided but is not valid RPC format" in {
    val underTest = normalRoute

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
    val underTest = normalRoute

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
      Rpc[TestMethodParams, TestMethodSuccess]("test_method2").serve(params =>
        TestMethodSuccess(params.userId.length).asRight[RpcError].toEitherT[Future]
      ) ~ normalRoute

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

  it should "return a InvalidParametersError when valid RPC format is provided but the parameters do not match the method" in {
    val underTest = normalRoute

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

      root.error.code.int.getOption(json).value shouldBe InvalidParametersError.Code
    }
  }

  it should "return a custom error when an RPC results in an unhandled exception" in {
    val underTest =
      Rpc[TestMethodParams, TestMethodSuccess]("test_method1").serve(_ =>
        EitherT[Future, RpcError, TestMethodSuccess](Future.failed(new Exception("Heck")))
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
      val response = responseAs[FailureRpcResponse]
      val error = response.error

      root.message.string.getOption(error.data.value).value shouldBe "Heck"
      root.stackTrace.arr.getOption(error.data.value).value should not be empty
    }
  }

  it should "return a custom error when an RPC results in a known error" in {

    val underTest =
      Rpc[TestMethodParams, TestMethodSuccess]("test_method1").serve(_ =>
        EitherT.leftT[Future, TestMethodSuccess](CustomError(-32005, "Heck"))
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
      val response = responseAs[FailureRpcResponse]
      val error = response.error

      error.message shouldBe "Heck"
    }
  }

  private def normalRoute: Route = {
    val rpc = Rpc[TestMethodParams, TestMethodSuccess]("test_method1")
    rpc.serve(params => TestMethodSuccess(params.userId.length).asRight[RpcError].toEitherT[Future])

  }

}

object RpcSpec {

  implicit val decodeParams: Decoder[TestMethodParams] =
    deriveDecoder[TestMethodParams]

  implicit val encodeParams: Encoder[TestMethodParams] =
    deriveEncoder[TestMethodParams]

  implicit val encodeTestMethod1Success: Encoder[TestMethodSuccess] =
    deriveEncoder[TestMethodSuccess]

}

case class TestMethodParams(userId: String)

case class TestMethodSuccess(value: Int)
