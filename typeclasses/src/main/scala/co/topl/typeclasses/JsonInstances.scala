package co.topl.typeclasses

import io.circe.Json

trait JsonInstances {
  implicit val doubleToJson: Double => Json = Json.fromDoubleOrNull(_)
  implicit val longToJson: Long => Json = Json.fromLong(_)
  implicit val intToJson: Int => Json = Json.fromInt(_)
  implicit val stringToJson: String => Json = Json.fromString(_)
  implicit val mapToJsonMap: Map[AnyVal, String] => Map[AnyVal, Json] = _.map { case (k, v) => k -> Json.fromString(v) }

  implicit val stringMapToJsonMap: Map[String, String] => Map[String, Json] = _.map { case (k, v) =>
    k -> Json.fromString(v)
  }
}
