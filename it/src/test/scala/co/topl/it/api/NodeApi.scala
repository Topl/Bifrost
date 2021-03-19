package co.topl.it.api

import akka.Done
import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, HttpRequest, StatusCode}
import co.topl.it.util.BifrostDockerNode
import com.spotify.docker.client.DockerClient
import org.slf4j.{Logger, LoggerFactory}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

class NodeRpcApi(host: String, rpcPort: Int)(implicit system: ActorSystem) {

  import system.dispatcher

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} host=$host rpcPort=$rpcPort")

  implicit def scheduler: Scheduler = system.scheduler

  def call(request: HttpRequest): Future[StrictHttpResponse] =
    akka.pattern.retry(
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
      .andThen {
        case Success(strict) =>
          log.debug(s"Request: ${request.uri} \n Response: ${strict.body}")
      }

  def post(path: String, data: String, f: HttpRequest ⇒ HttpRequest = identity): Future[StrictHttpResponse] =
    call(
      f(
        HttpRequest()
          .withMethod(HttpMethods.POST)
          .withUri(s"http://$host:$rpcPort$path")
          .withEntity(ContentTypes.`application/json`, data)
      )
    )

  def rpcFormat(method: String, params: String = "[{}]"): String =
    s"""
       |{
       |  "jsonrpc": "2.0",
       |  "id": "1",
       |  "method": "$method",
       |  "params": $params
       |}
       """.stripMargin

  def waitForStartup(): Future[Done] = {
    val data = rpcFormat("debug_myBlocks")
    post("/", data).map(_ => Done)
  }
}

object NodeRpcApi {

  def apply(node: BifrostDockerNode)(implicit system: ActorSystem, dockerClient: DockerClient): NodeRpcApi = {
    val host = dockerClient.inspectContainer(node.containerId).networkSettings().ipAddress()
    new NodeRpcApi(host, BifrostDockerNode.RpcPort)
  }
}

case class StrictHttpResponse(statusCode: StatusCode, headers: Map[String, String], body: String)
