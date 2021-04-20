package co.topl.it.util

import akka.Done
import akka.actor.{ActorSystem, Scheduler}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.RawHeader
import akka.stream.scaladsl.{Sink, Source}
import cats.data.{EitherT, NonEmptyList}
import cats.implicits._
import co.topl.utils.Int128
import com.spotify.docker.client.DockerClient
import io.circe._
import io.circe.generic.semiauto._
import io.circe.parser._
import io.circe.syntax._
import org.slf4j.{Logger, LoggerFactory}
import scorex.crypto.hash.{Blake2b256, Digest32}
import scorex.util.encode.Base58

import java.util.UUID
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Success, Try}

case class NodeRpcApi(host: String, rpcPort: Int)(implicit system: ActorSystem) {

  import NodeRpcApi._
  import system.dispatcher

  protected val log: Logger = LoggerFactory.getLogger(s"${getClass.getName} host=$host rpcPort=$rpcPort")

  implicit def scheduler: Scheduler = system.scheduler

  def retry[T](
    f:               () => Future[T],
    attemptInterval: FiniteDuration = 1.seconds,
    timeout:         FiniteDuration,
    attempts:        Int = 20
  ): Future[T] =
    Source
      .tick(Duration.Zero, attemptInterval, {})
      .take(attempts)
      .takeWithin(timeout)
      .mapAsync(1)(_ => f().map(Success(_)).recover { case e => Failure(e) })
      .collect { case Success(value) => value }
      .runWith(Sink.head)

  def call(request: HttpRequest): Future[StrictHttpResponse] =
    retry(
      () =>
        Http()
          .singleRequest(request)
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
          ),
      timeout = 30.seconds
    )
      .andThen { case Success(strict) =>
        log.debug(s"Request: ${request.uri} \n Response: ${strict.body}")
      }

  def post(path: String, data: String, f: HttpRequest ⇒ HttpRequest = identity): Future[StrictHttpResponse] =
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

  def pollUntilHeight(targetHeight: Int128, period: FiniteDuration = 200.milli)(implicit
    scheduler:                      Scheduler
  ): Future[Either[NodeApiError, Done]] =
    Source
      .tick(Duration.Zero, period, {})
      .mapAsync(1)(_ => Topl.head())
      .collect {
        case Right(response) if response.height >= targetHeight =>
          Right(Done)
      }
      .runWith(Sink.head)

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

    def stopForging(): Future[Either[NodeApiError, Done]] =
      EitherT(rpc("admin_stopForging"))
        .map(_ => Done)
        .value

    def updateRewardsAddress(address: String): Future[Either[NodeApiError, String]] =
      EitherT(rpc("admin_updateRewardsAddress", NonEmptyList.of(Map("address" -> address).asJson)))
        .subflatMap(_.hcursor.downField("result").downField("rewardsAddress").as[String].leftMap(JsonDecodingError))
        .value
  }

  object Topl {

    def head(): Future[Either[NodeApiError, Responses.HeadResponse]] =
      EitherT(rpc("topl_head"))
        .subflatMap(_.hcursor.downField("result").as[Responses.HeadResponse].leftMap(JsonDecodingError))
        .value

    object Responses {
      case class HeadResponse(height: Int128, score: Long, bestBlockId: String)

      implicit val headResponseDecoder: Decoder[HeadResponse] =
        Decoder.forProduct3("height", "score", "bestBlockId")(HeadResponse.apply)

    }
  }
}

object NodeRpcApi {

  val ApiKey = "integration-test-key"
  val ApiKeyHash: Digest32 = Blake2b256(ApiKey)
  val ApiKeyHashBase58: String = Base58.encode(ApiKeyHash)

  def apply(node: BifrostDockerNode)(implicit system: ActorSystem, dockerClient: DockerClient): NodeRpcApi = {
    val host = dockerClient.inspectContainer(node.containerId).networkSettings().ipAddress()
    new NodeRpcApi(host, BifrostDockerNode.RpcPort)
  }

  implicit val rpcInEncoder: Encoder[RpcIn] =
    deriveEncoder[RpcIn]

  implicit val int128Decoder: Decoder[Int128] =
    Decoder.decodeString
      .emapTry(v => Try(Int128(v)))
      .or(Decoder.decodeBigInt.emapTry(v => Try(Int128(v))))
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
