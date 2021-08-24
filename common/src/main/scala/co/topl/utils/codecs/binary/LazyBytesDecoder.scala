package co.topl.utils.codecs.binary

import cats.implicits._
import cats.data.ValidatedNec
import co.topl.utils.codecs.FromBytes
import simulacrum.typeclass

@typeclass
trait LazyBytesDecoder[A] {
  def decodeLazy(bytes: LazyList[Byte]): DecoderResult[A]

  def map[B](f: (A, LazyList[Byte]) => (B, LazyList[Byte])): LazyBytesDecoder[B] =
    bytes =>
      for {
        aResult <- decodeLazy(bytes)
      } yield f(aResult._1, aResult._2)
}

object LazyBytesDecoder {

  trait Implicits {

    implicit def toFromBytes[T: LazyBytesDecoder]: FromBytes[DecoderFailure, T] = new FromBytes[DecoderFailure, T] {

      override def decode(encoded: Array[Byte]): ValidatedNec[DecoderFailure, T] =
        LazyBytesDecoder[T].decodeLazy(LazyList.from(encoded)).map(_._1).toValidatedNec
    }
  }

  object implicits extends Implicits

}
