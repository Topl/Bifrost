package requests

trait ApiResponse {

  val success: Boolean
  val data: Json

  def toJson: Json = Map(
    "success" -> success.asJson,
    "data"    -> data
  ).asJson

}

case class SuccessResponse(override val data: Json, id: String) extends ApiResponse {
  override val success: Boolean = true

  override def toJson: Json = Map(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "result"  -> data
  ).asJson
}

case class ErrorResponse(e: Throwable, code: Int, id: String, verbose: Boolean = false) extends ApiResponse {
  override val success: Boolean = false
  override val data: Json = e.getMessage.asJson

  val trace: Option[(String, Json)] =
    if (verbose) Some("trace" -> e.getStackTrace.map(f => f.toString.asJson).asJson) else None

  e.printStackTrace()

  override def toJson: Json = Map(
    "jsonrpc" -> "2.0".asJson,
    "id"      -> id.asJson,
    "error" -> (Seq(
      "code"    -> code.asJson,
      "message" -> data
    ) ++ trace).toMap.asJson
  ).asJson
}
