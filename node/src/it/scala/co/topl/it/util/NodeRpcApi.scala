package co.topl.it.util

import akka.Done
import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import cats.data.{EitherT, NonEmptyList}
import cats.implicits._
import co.topl.crypto.hash.{blake2b256, Digest32}
import co.topl.utils.AsBytes.Implicits._
import co.topl.utils.encode.Base58
import com.spotify.docker.client.DockerClient
import io.circe._
import io.circe.generic.auto._
import io.circe.parser._
import io.circe.syntax._
import org.slf4j.{Logger, LoggerFactory}

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.util.Success

case class NodeRpcApi(host: String, rpcPort: Int)(implicit system: ActorSystem) {

  import system.dispatcher

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} host=$host rpcPort=$rpcPort")

  implicit def scheduler: Scheduler = system.scheduler

  def call(request: HttpRequest): Future[StrictHttpResponse] =
    akka.pattern
      .retry(
        () => Http().singleRequest(request),
        attempts = 20,
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
        log.debug(s"Request: ${request.uri} \n Response: ${strict.body}")
      }

  def post(path: String, data: String, f: HttpRequest => HttpRequest = identity): Future[StrictHttpResponse] =
    call(
      f(
        HttpRequest()
          .withMethod(HttpMethods.POST)
          .withUri(s"http://$host:$rpcPort$path")
          .withHeaders(RawHeader("x-api-key", NodeRpcApi.ApiKey))
          .withEntity(ContentTypes.`application/json`, data)
      )
    )

  def rpc(
    method: String,
    params: NonEmptyList[Json] = NonEmptyList.of(Json.obj())
  ): Future[Either[NodeApiError, Json]] =
    EitherT
      .right(post("/", encodeRpcBody(method, params)))
      .subflatMap(response =>
        response.statusCode match {
          case StatusCodes.OK => Right(response)
          case _              => Left(UnexpectedHttpResponseError(response))
        }
      )
      .subflatMap(response => parse(response.body).left.map(JsonParsingError))
      .value

  def encodeRpcBody(method: String, params: NonEmptyList[Json]): String =
    RpcIn(
      method = method,
      params = params
    ).asJson.toString()

  def waitForStartup(): Future[Either[NodeApiError, Done]] =
    EitherT(Debug.myBlocks()).map(_ => Done).value

  object Debug {

    def myBlocks(): Future[Either[NodeApiError, Long]] =
      EitherT(rpc("debug_myBlocks"))
        .subflatMap(_.hcursor.downField("result").downField("count").as[Long].leftMap(JsonDecodingError))
        .value

    def generators(): Future[Either[NodeApiError, Map[String, Long]]] =
      EitherT(rpc("debug_generators"))
        .subflatMap(_.hcursor.downField("result").as[Map[String, Long]].leftMap(JsonDecodingError))
        .value
  }

  object Admin {

    def listOpenKeyfiles(): Future[Either[NodeApiError, List[String]]] =
      EitherT(rpc("admin_listOpenKeyfiles"))
        .subflatMap(_.hcursor.downField("result").downField("unlocked").as[List[String]].leftMap(JsonDecodingError))
        .value

    def lockKeyfile(address: String): Future[Either[NodeApiError, Done]] =
      EitherT(rpc("admin_lockKeyfile", NonEmptyList.of(Map("address" -> address).asJson)))
        .map(_ => Done)
        .value

    def startForging(): Future[Either[NodeApiError, Done]] =
      EitherT(rpc("admin_startForging"))
        .map(_ => Done)
        .value
  }
}

object NodeRpcApi {

  val ApiKey = "integration-test-key"
  val ApiKeyHash: Digest32 = blake2b256(ApiKey)
  val ApiKeyHashBase58: String = Base58.encode(ApiKeyHash)

  def apply(node: BifrostDockerNode)(implicit system: ActorSystem, dockerClient: DockerClient): NodeRpcApi = {
    val host = dockerClient.inspectContainer(node.containerId).networkSettings().ipAddress()
    new NodeRpcApi(host, BifrostDockerNode.RpcPort)
  }
}

case class StrictHttpResponse(statusCode: StatusCode, headers: Map[String, String], body: String)

case class RpcIn(
  jsonrpc: String = "2.0",
  id:      String = UUID.randomUUID().toString,
  method:  String,
  params:  NonEmptyList[Json]
)

sealed trait NodeApiError
case class JsonParsingError(parsingFailure: ParsingFailure) extends NodeApiError
case class JsonDecodingError(decodingFailure: DecodingFailure) extends NodeApiError
case class UnexpectedHttpResponseError(strictHttpResponse: StrictHttpResponse) extends NodeApiError
case class RpcError(id: String, code: Int, message: String, trace: List[String]) extends NodeApiError
