package co.topl.utils.codecs.binary

import cats.implicits._
import scala.annotation.tailrec

object ULongSerde {

  type ULong = Long

  /**
   * Recursive function to parse an unsigned `Long` from a list of bytes using a VLQ method.
   *
   * Original Java Source:
   * http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java
   * /core/src/main/java/com/google/protobuf/CodedInputStream.java#L2653
   *
   * Faster Java Source:
   * http://github.com/google/protobuf/blob/a7252bf42df8f0841cf3a0c85fdbf1a5172adecb/java
   * /core/src/main/java/com/google/protobuf/CodedInputStream.java#L1085
   *
   * @param currentResult the current result as of this step
   * @param shift the current shift to apply to the next byte
   * @param remainingBytes the remaining bytes to parse
   * @param iteration the current iteration of the recursive loop
   * @return a `ParseResult` with a tuple of the parsed `Long` and remaining bytes if successful
   */
  @tailrec
  private def parseHelper(
    currentResult:  ULong,
    shift:          Int,
    remainingBytes: LazyList[Byte],
    iteration:      Int
  ): ParseResult[ULong, LazyList[Byte]] =
    (iteration, remainingBytes) match {
      case (i, _) if i >= 64                          => (currentResult, remainingBytes).asRight
      case (_, (head #:: tail)) if (head & 0x80) == 0 => (currentResult, remainingBytes).asRight
      case (_, (head #:: tail)) =>
        parseHelper(currentResult | ((head & 0x7f).toLong << shift), shift + 7, tail, iteration + 1)
      case _ => ParseFailure.asLeft
    }

  /**
   * Attempts to parse a `Long` value from a given list of bytes
   * @param from the bytes to parse a long from
   * @return a `ParseResult` with a tuple of the parsed `Long` and remaining bytes if successful
   */
  def parse(from: LazyList[Byte]): ParseResult[ULong, LazyList[Byte]] =
    parseHelper(currentResult = 0, shift = 0, remainingBytes = from, iteration = 0)
}
