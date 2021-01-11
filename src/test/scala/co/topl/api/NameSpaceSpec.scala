package co.topl.api

import akka.http.scaladsl.server.Route
import akka.util.ByteString
import co.topl.http.HttpService
import co.topl.http.api.ApiEndpoint
import co.topl.http.api.endpoints.{DebugApiEndpoint, KeyManagementApiEndpoint, NodeViewApiEndpoint, TransactionApiEndpoint, UtilsApiEndpoint}
import co.topl.settings.AppSettings
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.circe.parser.parse

class NameSpaceSpec extends AnyWordSpec
  with Matchers
  with RPCMockState {

  val rpcSettingsFalse: AppSettings = rpcSettings.copy(
    rpcApi = rpcSettings.rpcApi.copy(
      namespaceSelector = rpcSettings.rpcApi.namespaceSelector.copy(
        topl = false,
        util = false,
        admin = false,
        debug = false
      )
    )
  )

  val rpcSettingsTrue: AppSettings = rpcSettings.copy(
    rpcApi = rpcSettings.rpcApi.copy(
      namespaceSelector = rpcSettings.rpcApi.namespaceSelector.copy(
        topl = true,
        util = true,
        admin = true,
        debug = true
      )
    )
  )

  val apiRoutes: Seq[ApiEndpoint] = Seq(
    UtilsApiEndpoint(rpcSettings.rpcApi, appContext),
    KeyManagementApiEndpoint(rpcSettings.rpcApi, appContext, forgerRef),
    NodeViewApiEndpoint(rpcSettings.rpcApi, appContext, nodeViewHolderRef),
    TransactionApiEndpoint(rpcSettings.rpcApi, appContext, nodeViewHolderRef),
    DebugApiEndpoint(rpcSettings.rpcApi, appContext, nodeViewHolderRef, forgerRef)
  )

  val httpServiceFalse = HttpService(apiRoutes, rpcSettingsFalse.rpcApi)
  val routeFalse: Route = httpServiceFalse.compositeRoute

  val httpServiceTrue = HttpService(apiRoutes, rpcSettingsTrue.rpcApi)
  val routeTrue: Route = httpServiceTrue.compositeRoute

  "debug RPC with namespaces turned on" should {
    "Refuse debug requests with undefined method and return the correct errors" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "debug_empty",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> routeTrue ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "code").head.asNumber.get.toInt.get == 500 shouldBe true
        (res \\ "message").head.asString.get == "Service handler not found for method: debug_empty" shouldBe true
      }
    }
  }

  "debug RPC with namespaces turned off" should {
    "Refuse debug requests with correct method and return the correct errors" in {
      val requestBody = ByteString(
        s"""
           |{
           |   "jsonrpc": "2.0",
           |   "id": "1",
           |   "method": "debug_myBlocks",
           |   "params": [{}]
           |}
        """.stripMargin)

      httpPOST(requestBody) ~> routeFalse ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "code").head.asNumber.get.toInt.get == 500 shouldBe true
        (res \\ "message").head.asString.get == "Service handler not found for method: debug_myBlocks" shouldBe true
      }
    }
  }

  "debug RPC with namespaces turned off or on" should {
    "Refuse any request with incomplete Json body and return the correct errors" in {
      val requestBody = ByteString("")

      httpPOST(requestBody) ~> routeTrue ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "code").head.asNumber.get.toInt.get == 400 shouldBe true
        (res \\ "message").head.asString.get == "Unable to parse Json body" shouldBe true
      }

      httpPOST(requestBody) ~> routeFalse ~> check {
        val res = parse(responseAs[String]) match {case Right(re) => re; case Left(ex) => throw ex}
        (res \\ "code").head.asNumber.get.toInt.get == 400 shouldBe true
        (res \\ "message").head.asString.get == "Unable to parse Json body" shouldBe true
      }
    }
  }
}
