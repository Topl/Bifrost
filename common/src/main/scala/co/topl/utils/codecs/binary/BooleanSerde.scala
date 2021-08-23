package co.topl.utils.codecs.binary

import cats.implicits._

object BooleanSerde {
  def parse(from: LazyList[Byte]): ParseResult[Boolean, LazyList[Byte]] =
    from match {
      case head #:: tail => (head == 0x01, tail).asRight
      // in the case when the byte list is empty
      case _ => ParseFailure.asLeft
    }
}
