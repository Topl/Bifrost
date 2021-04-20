package co.topl.api

import akka.http.scaladsl.model.StatusCodes.{BadRequest, InternalServerError}
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.HttpService
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints._
import co.topl.settings.AppSettings
import io.circe.parser.parse
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NameSpaceSpec extends AnyWordSpec with Matchers with RPCMockState {

  def createRoute(args: Boolean*): Route = {
    val newRpcSettings: AppSettings = rpcSettings.copy(
      rpcApi = rpcSettings.rpcApi.copy(
        namespaceSelector = rpcSettings.rpcApi.namespaceSelector.copy(
          topl = args(0),
          util = args(1),
          admin = args(2),
          debug = args(3)
        )
      )
    )

    val newApiRoutes: Seq[ApiEndpoint] = Seq(
      UtilsApiEndpoint(newRpcSettings.rpcApi, appContext),
      AdminApiEndpoint(settings.rpcApi, appContext, forgerRef, keyManagerRef),
      NodeViewApiEndpoint(newRpcSettings.rpcApi, appContext, nodeViewHolderRef),
      TransactionApiEndpoint(newRpcSettings.rpcApi, appContext, nodeViewHolderRef),
      DebugApiEndpoint(newRpcSettings.rpcApi, appContext, nodeViewHolderRef, forgerRef)
    )

    val newHttpService = HttpService(newApiRoutes, newRpcSettings.rpcApi)
    newHttpService.compositeRoute
  }

  val routeTrue: Route = createRoute(true, true, true, true)
  val routeFalse: Route = createRoute(false, false, false, false)
  val routeDefault: Route = createRoute(true, true, true, false)
  val randBools: Seq[Boolean] = (for (i <- 1 to 4) yield scala.util.Random.nextBoolean())
  val routeRandom: Route = createRoute(randBools: _*)

  "debug RPC with namespaces turned on" should {
    "Refuse debug requests with undefined method and return the correct errors" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "debug_empty",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> routeTrue ~> check {
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(InternalServerError.intValue)
        message shouldEqual Right("Service handler not found for method: debug_empty")
      }
    }
  }

  "debug RPC with namespaces turned off" should {
    "Refuse debug requests with correct method and return the correct errors" in {
      val requestBody = ByteString(s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "debug_myBlocks",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> routeFalse ~> check {
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(InternalServerError.intValue)
        message shouldEqual Right("Service handler not found for method: debug_myBlocks")
      }

      httpPOST(requestBody) ~> routeDefault ~> check {
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(InternalServerError.intValue)
        message shouldEqual Right("Service handler not found for method: debug_myBlocks")
      }
    }
  }

  "debug RPC with namespaces turned off or on" should {
    "Refuse any request with incomplete Json body and return the correct errors" in {
      val requestBody = ByteString("")

      httpPOST(requestBody) ~> routeRandom ~> check {
        val res = parse(responseAs[String]) match { case Right(re) => re; case Left(ex) => throw ex }
        val code = res.hcursor.downField("error").get[Int]("code")
        val message = res.hcursor.downField("error").get[String]("message")

        code shouldEqual Right(BadRequest.intValue)
        message shouldEqual Right("Unable to parse Json body")
      }
    }
  }
}
