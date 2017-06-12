package bifrost.api.http

import java.io.{PrintWriter, StringWriter}

import io.circe.Json
import io.circe.syntax._
import scorex.core.api.http.ScorexApiResponse

/**
  * Created by cykoz on 6/12/2017.
  */
case class BifrostSuccessResponse(override val data: Json, id: String) extends ScorexApiResponse {
  override val success: Boolean = true
  override def toJson: Json = Map(
    "jsonrpc" -> "2.0".asJson,
    "id" -> id.asJson,
    "result" -> data
  ).asJson
}

case class BifrostErrorResponse(e: Throwable, id: String) extends ScorexApiResponse {
  override val success: Boolean = false
  override val data: Json = (e.getMessage).asJson
  e.printStackTrace()
//  val sw = new StringWriter
//  e.printStackTrace(new PrintWriter(sw))
  override def toJson: Json = Map(
    "jsonrpc" -> "2.0".asJson,
    "id" -> id.asJson,
    "error" -> Map(
      "code" -> 500.asJson,
      "message" -> data
    ).asJson
  ).asJson
}
