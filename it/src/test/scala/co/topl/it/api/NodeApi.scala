package co.topl.it.api

import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest, StatusCode}
import org.asynchttpclient._
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

trait NodeApi {

  implicit def ec: ExecutionContext

  implicit def system: ActorSystem

  def restAddress: String

  def nodeRpcPort: Int

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} $restAddress")

  implicit def scheduler: Scheduler = system.scheduler

  def post(path: String, data: String, f: HttpRequest ⇒ HttpRequest = identity): Future[StrictHttpResponse] = {
    val request =
      f(
        HttpRequest()
          .withMethod(HttpMethods.POST)
          .withUri(s"http://$restAddress:$nodeRpcPort$path")
          .withEntity(ContentTypes.`application/json`, data)
      )
    akka.pattern
      .retry(
        () => Http().singleRequest(request),
        attempts = 100,
        1.seconds
      )
      .flatMap(response =>
        response.entity
          .toStrict(1.second)
          .map(entity =>
            StrictHttpResponse(
              response.status,
              response.headers.map(header => header.name() -> header.value()).toMap,
              entity.data.utf8String
            )
          )
      )
      .andThen { case Success(strict) =>
        log.info(s"Request: ${request.uri} \n Response: ${strict.body}")
      }
  }

  def rpcFormat(method: String, params: String = "[{}]"): String =
    s"""
       |{
       |  "jsonrpc": "2.0",
       |  "id": "1",
       |  "method": "$method",
       |  "params": $params
       |}
       """.stripMargin

  def waitForStartup: Future[this.type] = {
    val data = rpcFormat("debug_myBlocks")
    post("/", data).map(_ ⇒ this)
  }
}

object NodeApi {

  case class UnexpectedStatusCodeException(request: Request, response: Response)
      extends Exception(
        s"Request: ${request.getUrl}\n Unexpected status code (${response.getStatusCode}): " +
        s"${response.getResponseBody}"
      )

}

case class StrictHttpResponse(statusCode: StatusCode, headers: Map[String, String], body: String)
