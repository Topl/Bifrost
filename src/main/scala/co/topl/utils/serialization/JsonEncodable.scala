package co.topl.utils.serialization

import io.circe.Json

trait JsonEncodable {
  val json: Json
}
