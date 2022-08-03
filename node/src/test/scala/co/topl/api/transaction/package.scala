package co.topl.api

import cats.implicits._
import io.circe.{ACursor, Decoder, HCursor}
import io.circe.parser.parse

package object transaction {

  /**
   * Traverses the provided json [[String]] using the provided cursor path.
   * @param json the JSON to parse and traverse
   * @param path the path to follow down the JSON tree
   * @tparam T the type of value to attempt to decode at the leaf of the path
   * @return if successful, a value of [[T]], otherwise a [[String]]
   */
  def traverseJsonPath[T: Decoder](json: String, path: HCursor => ACursor): Either[String, T] =
    for {
      json   <- parse(json).leftMap(_.toString)
      result <- path(json.hcursor).as[T].leftMap(_.message)
    } yield result
}
