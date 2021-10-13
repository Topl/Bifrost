package co.topl.utils.codecs.binary.valuetypes

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}
import spire.ClassTag

object StaticArrayCodec {

  def codec[T: Codec: ClassTag](size: Int): Codec[Array[T]] = new Codec[Array[T]] {

    override def decode(bits: BitVector): Attempt[DecodeResult[Array[T]]] =
      (0 until size)
        .foldLeft(Attempt.successful(DecodeResult(List[T](), bits))) {
          case (Attempt.Successful(previousResult), _) =>
            Codec[T]
              .decode(previousResult.remainder)
              .map(result => result.map(previousResult.value :+ _))
          case (failure, _) => failure
        }
        .map(_.map(_.toArray))

    override def encode(value: Array[T]): Attempt[BitVector] =
      value.foldLeft(Attempt.successful(BitVector.empty)) {
        case (Attempt.Successful(bits), next) =>
          Codec[T].encode(next).map(bits ++ _)
        case (failure, _) => failure
      }

    override def sizeBound: SizeBound = Codec[T].sizeBound * size
  }

  trait Codecs {
    def staticArrayCodec[T: Codec: ClassTag](size: Int): Codec[Array[T]] = codec(size)
  }

  object codecs extends Codecs
}
