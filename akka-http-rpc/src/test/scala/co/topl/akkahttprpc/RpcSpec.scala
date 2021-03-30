package co.topl.akkahttprpc

import akka.http.scaladsl.testkit.ScalatestRouteTest
import cats.data.{EitherT, NonEmptyMap}
import cats.implicits._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import org.scalatest.EitherValues
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.matchers.should.Matchers

import scala.concurrent.Future

class RpcSpec extends AnyFlatSpecLike with ScalatestRouteTest with EitherValues with Matchers {

  behavior of "Rpc"

  implicit val decodeParams: Decoder[TestMethod1Params] =
    deriveDecoder[TestMethod1Params]

  implicit val encodeParams: Encoder[TestMethod1Params] =
    deriveEncoder[TestMethod1Params]

  import JsonFailureSupport.throwableEncoder

  implicit val encodeTestMethod1Error: Encoder[TestMethod1Error] =
    deriveEncoder[TestMethod1Error]

  implicit val encodeTestMethod1Success: Encoder[TestMethod1Success] =
    deriveEncoder[TestMethod1Success]

  implicit val errorToRpcError: RpcErrorEncoder[TestMethod1Error] =
    e => ServerError.fromThrowable(32034, "Test Exception", e.throwable)

  it should "create akka-http Routes using RPC handlers" in {
    val method1Handler: RpcHandler[_, _, _] =
      new RpcHandler[TestMethod1Params, TestMethod1Error, TestMethod1Success] {
        override def apply(params: TestMethod1Params): EitherT[Future, TestMethod1Error, TestMethod1Success] =
          EitherT.pure[Future, TestMethod1Error](TestMethod1Success(params.userId.length))
      }

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

  case class TestMethod1Params(userId: String)
  case class TestMethod1Error(throwable: Throwable)

  case class TestMethod1Success(value: Int)

}
