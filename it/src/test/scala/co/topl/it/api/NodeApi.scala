package co.topl.it.api

import java.io.IOException
import java.util.concurrent.TimeoutException

import bifrost.it.util.TimerExt
import io.netty.util.{HashedWheelTimer, Timer}
import org.asynchttpclient._
import org.asynchttpclient.Dsl.{post ⇒ _post}
import org.asynchttpclient.util.HttpConstants
import org.slf4j.{Logger, LoggerFactory}

import scala.compat.java8.FutureConverters.CompletionStageOps
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}

trait NodeApi {

  implicit def ec: ExecutionContext

  def restAddress: String

  def nodeRpcPort: Int

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} $restAddress")

  protected val client: AsyncHttpClient = new DefaultAsyncHttpClient

  protected val timer: Timer = new HashedWheelTimer()

  def post(path: String, data: String ,f: RequestBuilder ⇒ RequestBuilder = identity): Future[Response] =
    retrying(
      f(_post(s"http://$restAddress:$nodeRpcPort$path")
        .setHeader("Content-Type", "application/json")
        .setBody(data)
      ).build())

  def rpcFormat(method: String, params: String = "[{}]"): String =
    s"""
       |{
       |  "jsonrpc": "2.0",
       |  "id": "1",
       |  "method": "$method",
       |  "params": $params
       """.stripMargin

  def waitForStartup: Future[this.type] = {
    val data = rpcFormat("info")
    post("/debug", data).map(_ ⇒ this)
  }


  def retrying(request: Request,
               interval: FiniteDuration = 1.second,
               statusCode: Int = HttpConstants.ResponseStatusCodes.OK_200): Future[Response] = {
    def executeRequest: Future[Response] = {
      log.trace(s"Executing request '$request'")
      client.executeRequest(request, new AsyncCompletionHandler[Response] {
        override def onCompleted(response: Response): Response = {
          if (response.getStatusCode == statusCode) {
            log.debug(s"Request: ${request.getUrl} \n Response: ${response.getResponseBody}")
            response
          } else {
            log.debug(s"Request:  ${request.getUrl} \n Unexpected status code(${response.getStatusCode}): " +
              s"${response.getResponseBody}")
            throw NodeApi.UnexpectedStatusCodeException(request, response)
          }
        }
      }).toCompletableFuture.toScala
        .recoverWith {
          case e@(_: IOException | _: TimeoutException) =>
            log.debug(s"Failed to execute request '$request' with error: ${e.getMessage}")
            timer.schedule(executeRequest, interval)
        }
    }

    executeRequest
  }
}

object NodeApi {

  case class UnexpectedStatusCodeException(request: Request, response: Response)
    extends Exception(s"Request: ${request.getUrl}\n Unexpected status code (${response.getStatusCode}): " +
      s"${response.getResponseBody}")
}
