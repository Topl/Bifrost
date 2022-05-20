package co.topl.codecs.bytes.scodecs.valuetypes

import cats.ApplicativeError
import cats.data.{Chain, NonEmptyChain}
import cats.implicits._
import co.topl.codecs.bytes.ZigZagEncoder._
import co.topl.codecs.bytes.scodecs.valuetypes.Constants._
import co.topl.codecs.bytes.scodecs.valuetypes.Types._
import scodec.bits.{BitVector, ByteVector}
import scodec.interop.cats._
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.collection.SortedSet
import scala.collection.immutable.{ListMap, ListSet}
import scala.reflect.ClassTag
import scala.util.Try

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

  def bytesCodec(size: Int): Codec[ByteVector] = byteArrayCodec(size).xmap(ByteVector(_), _.toArray)

  def byteArrayCodec(size: Int): Codec[Array[Byte]] = new ByteArrayCodec(size)

  val uByteCodec: Codec[UByte] =
    byteCodec
      .exmapc(byte => Attempt.successful(byte & 0xff))(ubyte =>
        Attempt
          .guard(ubyte >= 0 && ubyte <= 0xff, s"$ubyte is out of unsigned byte range")
          .map(_ => ubyte.toByte)
      )

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

  implicit val byteStringCodec: Codec[ByteString] =
    uByteCodec.consume[ByteString](size =>
      byteArrayCodec(size)
        .xmap(bytes => new String(bytes, stringCharacterSet), str => str.getBytes(stringCharacterSet))
    )(str => str.length.toByte)

  implicit val intStringCodec: Codec[IntString] =
    uIntCodec
      .exmap[Int](
        uInt =>
          if (uInt < Int.MinValue || uInt > Int.MaxValue)
            Attempt.failure(Err("UInt value is outside of valid range."))
          else
            Attempt.successful(uInt.toInt),
        v => Attempt.successful(v.toLong)
      )
      .consume[IntString](int =>
        byteArrayCodec(int)
          .xmap(bytes => new String(bytes, stringCharacterSet), str => str.getBytes(stringCharacterSet))
      )(str => str.length)

  implicit def tupleCodec[A: Codec, B: Codec]: Codec[(A, B)] =
    Codec[A].consume[(A, B)](a => Codec[B].xmap[(A, B)](b => (a, b), ab => ab._2))(ab => ab._1)

  implicit def tupleCodec[A: Codec, B: Codec, C: Codec]: Codec[(A, B, C)] =
    Codec[A].consume[(A, B, C)](a =>
      Codec[B].consume[(A, B, C)](b => Codec[C].xmap[(A, B, C)](c => (a, b, c), abc => abc._3))(abc => abc._2)
    )(abc => abc._1)

  def sizedArrayCodec[T: Codec: ClassTag](size: Int): Codec[Array[T]] = new Codec[Array[T]] {

    override def encode(value: Array[T]): Attempt[BitVector] =
      Attempt
        .guard(value.length == size, Err("invalid collection size"))
        .flatMap(_ =>
          value.foldLeft(Attempt.successful(BitVector.empty)) {
            case (Attempt.Successful(bits), item) => Codec[T].encode(item).map(bits ++ _)
            case (failure, _)                     => failure
          }
        )

    override def sizeBound: SizeBound = Codec[T].sizeBound * size

    override def decode(bits: BitVector): Attempt[DecodeResult[Array[T]]] = {
      var remaining = bits
      Attempt
        .fromTry(
          Try(
            Array.fill(size) {
              Codec[T].decode(remaining) match {
                case Attempt.Successful(value) =>
                  remaining = value.remainder
                  value.value
                case Attempt.Failure(cause) =>
                  throw new Exception(cause.messageWithContext)
              }
            }
          )
        )
        .map(DecodeResult(_, remaining))
    }
  }

  def sizedChainCodec[T: Codec](size: Int): Codec[Chain[T]] = new Codec[Chain[T]] {

    override def encode(value: Chain[T]): Attempt[BitVector] =
      Attempt
        .guard(value.length == size, Err(s"invalid collection size=${value.length} expected=$size"))
        .flatMap(_ => value.foldMapM(Codec[T].encode))

    override def sizeBound: SizeBound = Codec[T].sizeBound * size

    override def decode(bits: BitVector): Attempt[DecodeResult[Chain[T]]] =
      (0 until size).foldLeft(Attempt.successful(DecodeResult(Chain.empty[T], bits))) {
        case (Attempt.Successful(DecodeResult(listT, remaining)), _) =>
          Codec[T].decode(remaining).map(result => result.map(listT.append))
        case (failure, _) => failure
      }
  }

  implicit def chainCodec[T: Codec]: Codec[Chain[T]] =
    uIntCodec.consume(uInt => sizedChainCodec[T](uInt.toInt))(_.length)

  implicit def seqCodec[T: Codec]: Codec[Seq[T]] =
    chainCodec[T].xmap(_.toList, Chain.fromSeq)

  implicit def listCodec[T: Codec]: Codec[List[T]] =
    chainCodec[T].xmap(_.toList, Chain.fromSeq)

  implicit def vectorCodec[T: Codec]: Codec[Vector[T]] =
    chainCodec[T].xmap(_.toVector, Chain.fromSeq)

  implicit def setCodec[T: Codec]: Codec[Set[T]] =
    seqCodec[T].xmap(_.toSet, _.toSeq)

  implicit def listSetCodec[T: Codec]: Codec[ListSet[T]] =
    seqCodec[T].xmap(ListSet.empty[T] ++ _, _.toSeq)

  implicit def sortedSetCodec[T: Codec: Ordering]: Codec[SortedSet[T]] =
    seqCodec[T].xmap(SortedSet.empty[T] ++ _, _.toSeq)

  implicit def indexedSeqCodec[T: Codec]: Codec[IndexedSeq[T]] =
    seqCodec[T].xmap(_.toIndexedSeq, identity)

  implicit def nonEmptyChainCodec[T: Codec]: Codec[NonEmptyChain[T]] =
    chainCodec[T].exmap(
      t =>
        ApplicativeError
          .liftFromOption[Attempt](NonEmptyChain.fromChain(t), Err("Expected non-empty seq, but empty found")),
      _.toChain.pure[Attempt]
    )

  implicit def arrayCodec[T: Codec: ClassTag]: Codec[Array[T]] =
    uIntCodec.consume(uInt => sizedArrayCodec[T](uInt.toInt))(_.length)

  implicit def listMapCodec[A: Codec, B: Codec]: Codec[ListMap[A, B]] =
    seqCodec[(A, B)].xmap[ListMap[A, B]](ListMap.empty[A, B] ++ _, _.toSeq)

  implicit def mapCodec[A: Codec, B: Codec]: Codec[Map[A, B]] =
    listMapCodec[A, B].xmap(_.toMap, ListMap.empty[A, B] ++ _)

  implicit def optionCodec[T: Codec]: Codec[Option[T]] = new OptionCodec[T]
}
