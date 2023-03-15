package co.topl.codecs.bytes.scodecs.valuetypes

import cats.implicits._
import co.topl.codecs.bytes.ZigZagEncoder._
import co.topl.codecs.bytes.scodecs.valuetypes.Constants._
import co.topl.codecs.bytes.scodecs.valuetypes.Types._
import com.google.protobuf.ByteString
import scodec.bits.BitVector
import scodec.interop.cats._
import scodec.Attempt
import scodec.Codec
import scodec.DecodeResult
import scodec.Err
import scodec.SizeBound

trait ValuetypesCodecs {

  /**
   * A valuetype codec which encodes zero bytes and decodes zero bytes.
   * @param instance The instance to return when decoding
   */
  def emptyCodec[T](instance: T): Codec[T] =
    new Codec[T] {
      def decode(bits: BitVector): Attempt[DecodeResult[T]] = Attempt.successful(DecodeResult(instance, bits))

      def encode(value: T): Attempt[BitVector] = Attempt.successful(BitVector.empty)

      def sizeBound: SizeBound = SizeBound.exact(0)
    }

  implicit val byteCodec: Codec[Byte] = ByteCodec

  implicit val uLongCodec: Codec[ULong] = ULongFastCodec

  implicit val uIntCodec: Codec[UInt] =
    uLongCodec.exmapc[UInt](uLong =>
      if (uLong >= minUIntValue && uLong <= maxUIntValue) Attempt.successful(uLong)
      else Attempt.failure(Err("UInt value is outside of valid range."))
    )(uInt => Attempt.successful(uInt))

  private val trueByte: Byte = 0x01
  private val falseByte: Byte = 0x00

  implicit val boolCodec: Codec[Boolean] =
    byteCodec.xmap(byte => byte === trueByte, bool => if (bool) trueByte else falseByte)

  implicit val intCodec: Codec[Int] =
    uLongCodec.xmap(uLong => decodeZigZagInt(uLong.toInt), int => encodeZigZagInt(int))

  implicit val longCodec: Codec[Long] =
    uLongCodec.xmap(uLong => decodeZigZagLong(uLong), long => encodeZigZagLong(long))

  implicit val shortCodec: Codec[Short] =
    uLongCodec.xmap(uLong => decodeZigZagInt(uLong.toInt).toShort, short => encodeZigZagInt(short))

  implicit val uShortCodec: Codec[UShort] =
    uLongCodec.exmapc(uLong =>
      if (uLong >= minUShortValue && uLong <= maxUShortValue)
        Attempt.successful(uLong.toInt)
      else
        Attempt.failure(Err("UShort value is outside of valid range."))
    )(uShort => Attempt.successful(uShort))

  def byteStringCodecSized(size: Int): Codec[ByteString] =
    byteArrayCodecSized(size).xmap(ByteString.copyFrom, _.toByteArray)

  implicit val byteStringCodec: Codec[ByteString] =
    uIntCodec.consume(uInt => byteStringCodecSized(uInt.toInt))(_.size())

  def byteArrayCodecSized(size: Int): Codec[Array[Byte]] =
    new ByteArrayCodec(size)

  implicit val byteArrayCodec: Codec[Array[Byte]] =
    uIntCodec.consume(uInt => byteArrayCodecSized(uInt.toInt))(_.length)

  implicit def tupleCodec[A: Codec, B: Codec]: Codec[(A, B)] =
    Codec[A].consume[(A, B)](a => Codec[B].xmap[(A, B)](b => (a, b), ab => ab._2))(ab => ab._1)

  def sizedSeqCodec[T: Codec](size: Int): Codec[Seq[T]] = new Codec[Seq[T]] {

    override def encode(value: Seq[T]): Attempt[BitVector] =
      Attempt
        .guard(value.length == size, Err(s"invalid collection size=${value.length} expected=$size"))
        .flatMap(_ => value.foldMapM(Codec[T].encode))

    override def sizeBound: SizeBound = Codec[T].sizeBound * size

    override def decode(bits: BitVector): Attempt[DecodeResult[Seq[T]]] = {
      var remaining = bits
      val result =
        Seq.fill(size) {
          Codec[T].decode(remaining) match {
            case Attempt.Successful(value) =>
              remaining = value.remainder
              value.value
            case Attempt.Failure(f) =>
              return Attempt.Failure(f)
          }
        }
      Attempt.successful(DecodeResult(result, remaining))
    }
  }

  implicit def seqCodec[T: Codec]: Codec[Seq[T]] =
    uIntCodec.consume(uInt => sizedSeqCodec[T](uInt.toInt))(_.length)

  implicit def vectorCodec[T: Codec]: Codec[Vector[T]] =
    seqCodec[T].xmap(_.toVector, _.toSeq)

  implicit def optionCodec[T: Codec]: Codec[Option[T]] = new OptionCodec[T]
}
