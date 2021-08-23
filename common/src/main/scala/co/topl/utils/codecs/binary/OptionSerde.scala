package co.topl.utils.codecs.binary

import cats.implicits._

object OptionSerde {

  def parse[T: LazyBinaryParser](from: LazyList[Byte]): ParseResult[Option[T], LazyList[Byte]] =
    from match {
      case head #:: tail if head != 0 => LazyBinaryParser[T].parse(tail).map(result => (result._1.some, result._2))
      case head #:: tail if head == 0 => (None, tail).asRight
      case _                          => ParseFailure.asLeft
    }
}
