package co.topl.utils.serialization

import io.circe.Json

trait JsonSerializable {
  def json: Json
}
