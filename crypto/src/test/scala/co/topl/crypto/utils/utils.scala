package co.topl.crypto

import io.circe.generic.codec.DerivedAsObjectCodec.deriveCodec
import io.circe.generic.semiauto.deriveDecoder
import io.circe.{parser, Decoder}

package object utils {

  def randomBytes(length: Int = 32): Array[Byte] = {
    val r = new Array[Byte](length)
    new java.security.SecureRandom().nextBytes(r) // overrides r
    r
  }

  abstract class TestVector

  object TestVector {

    def read[A <: TestVector](fileName: String)(implicit decoder: Decoder[A]): List[A] = {
      val file = scala.io.Source.fromResource(fileName).getLines().mkString
      val vectorEither = for {
        json    <- parser.parse(file)
        vectors <- json.as[List[A]]
      } yield vectors

      vectorEither match {
        case Left(err)  => throw err
        case Right(vec) => vec
      }
    }
  }
}
