package bifrost.api.http

import io.circe.Json
import io.circe.syntax._

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

case class BifrostErrorResponse(e: Throwable, code: Int, id: String, verbose: Boolean = false) extends ScorexApiResponse {
  override val success: Boolean = false
  override val data: Json = e.getMessage.asJson
  val trace: Option[(String, Json)] = if(verbose) Some("trace" -> e.getStackTrace.map(f => f.toString.asJson).asJson) else None

  e.printStackTrace()

  override def toJson: Json = Map(
    "jsonrpc" -> "2.0".asJson,
    "id" -> id.asJson,
    "error" -> (Seq(
      "code" -> code.asJson,
      "message" -> data
    ) ++ trace).toMap.asJson
  ).asJson
}
