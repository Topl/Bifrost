package co.topl.utils.codecs

import cats.implicits._
import co.topl.utils.Int128
import co.topl.utils.serialization.ZigZagEncoder._
import simulacrum.typeclass
import co.topl.utils.Extensions.LongOps

import java.nio.ByteBuffer
import java.util
import scala.annotation.tailrec

package object binary {

  case object ParseFailure
  type ParseFailure = ParseFailure.type
  type ParseResult[A, B] = Either[ParseFailure, (A, B)]

  @typeclass
  trait LazyBinaryParser[T] {
    def parse(bytes: LazyList[Byte]): ParseResult[T, LazyList[Byte]]
  }

  @tailrec
  private[binary] def stringParsingHelper(
    size:      Int,
    iteration: Int,
    current:   List[Byte],
    remaining: LazyList[Byte]
  ): ParseResult[String, LazyList[Byte]] =
    if (size >= iteration) (new String(current.toArray), remaining).asRight
    else
      remaining match {
        case head #:: tail => stringParsingHelper(size, iteration + 1, current :+ head, tail)
        case _             => ParseFailure.asLeft
      }
}
