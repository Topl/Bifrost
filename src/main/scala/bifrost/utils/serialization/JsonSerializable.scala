package bifrost.utils.serialization

import io.circe.Json

trait JsonSerializable {
  def json: Json
}
