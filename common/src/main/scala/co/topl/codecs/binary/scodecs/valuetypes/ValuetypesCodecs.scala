package co.topl.codecs.binary.scodecs.valuetypes

import cats.implicits._
import co.topl.codecs.binary.ZigZagEncoder._
import co.topl.codecs.binary.scodecs.valuetypes.Constants._
import co.topl.codecs.binary.scodecs.valuetypes.Types._
import co.topl.utils.Extensions.LongOps
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

trait ValuetypesCodecs {
  implicit val byteCodec: Codec[Byte] = ByteCodec

  def bytesCodec(size: Int): Codec[Array[Byte]] = new BytesCodec(size)

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

  // JAA - for signed values, use Zig-Zag encoder to modify bits so the varint encoding scheme is more efficient
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

  implicit val int128Codec: Codec[Int128] =
    bytesCodec(Int128.numBytes).xmap(bytes => Int128(bytes), int => int.toByteArray)

  implicit val byteStringCodec: Codec[ByteString] =
    uByteCodec.consume[ByteString](size =>
      bytesCodec(size)
        .xmap(bytes => new String(bytes, stringCharacterSet), str => str.getBytes(stringCharacterSet))
    )(str => str.length.toByte)

  implicit val intStringCodec: Codec[IntString] =
    uIntCodec.consume[IntString](uInt =>
      bytesCodec(uInt.toIntExact)
        .xmap(bytes => new String(bytes, stringCharacterSet), str => str.getBytes(stringCharacterSet))
    )(str => str.length)

  implicit val latin1DataCodec: Codec[Latin1Data] =
    byteStringCodec.exmapc(byteString =>
      Latin1Data
        .validated(byteString)
        .map(data => Attempt.successful(data))
        .valueOr(errs => Attempt.failure(Err(errs.toString)))
    )(latin1Data => Attempt.successful(new String(latin1Data.value, stringCharacterSet)))

  implicit def tupleCodec[A: Codec, B: Codec]: Codec[(A, B)] =
    Codec[A].consume[(A, B)](a => Codec[B].xmap[(A, B)](b => (a, b), ab => ab._2))(ab => ab._1)

  implicit def tupleCodec[A: Codec, B: Codec, C: Codec]: Codec[(A, B, C)] =
    Codec[A].consume[(A, B, C)](a =>
      Codec[B].consume[(A, B, C)](b => Codec[C].xmap[(A, B, C)](c => (a, b, c), abc => abc._3))(abc => abc._2)
    )(abc => abc._1)

  def sizedListCodec[T: Codec](size: Int): Codec[List[T]] = new Codec[List[T]] {

    override def encode(value: List[T]): Attempt[BitVector] =
      (0 until size).foldLeft(Attempt.successful(BitVector.empty)) {
        case (Attempt.Successful(bits), index) =>
          for {
            item     <- value.get(index).map(Attempt.successful).getOrElse(Attempt.failure(Err("invalid list size")))
            itemBits <- Codec[T].encode(item)
          } yield bits ++ itemBits
        case (failure, _) => failure
      }

    override def sizeBound: SizeBound = Codec[T].sizeBound * size

    override def decode(bits: BitVector): Attempt[DecodeResult[List[T]]] =
      (0 until size).foldLeft(Attempt.successful(DecodeResult(List[T](), bits))) {
        case (Attempt.Successful(DecodeResult(listT, remaining)), _) =>
          Codec[T].decode(remaining).map(result => result.map(listT :+ _))
        case (failure, _) => failure
      }
  }

  implicit def listCodec[T: Codec]: Codec[List[T]] =
    uIntCodec.consume[List[T]](uInt => sizedListCodec(uInt.toInt))(listT => listT.length)

  implicit def setCodec[T: Codec]: Codec[Set[T]] =
    listCodec[T].xmap(list => list.toSet, set => set.toList)

  implicit def sortedSetCodec[T: Codec: Ordering]: Codec[SortedSet[T]] =
    listCodec[T].xmap(list => SortedSet(list: _*), sortedSet => sortedSet.toList)

  implicit def indexedSeqCodec[T: Codec]: Codec[IndexedSeq[T]] =
    listCodec[T].xmap(list => list.toIndexedSeq, seq => seq.toList)

  implicit def seqCodec[T: Codec]: Codec[Seq[T]] =
    listCodec[T].xmap(list => list.toSeq, seq => seq.toList)

  implicit def arrayCodec[T: Codec: ClassTag]: Codec[Array[T]] =
    listCodec[T].xmap(list => list.toArray, array => array.toList)

  implicit def listMapCodec[A: Codec, B: Codec]: Codec[ListMap[A, B]] =
    listCodec[(A, B)].xmap[ListMap[A, B]](list => ListMap(list: _*), listMap => listMap.toList)

  implicit def mapCodec[A: Codec, B: Codec]: Codec[Map[A, B]] =
    listMapCodec[A, B].xmap(listMap => listMap, map => ListMap(map.toList: _*))

  def sizedArrayCodec[T: Codec: ClassTag](size: Int): Codec[Array[T]] =
    sizedListCodec[T](size).xmap[Array[T]](listT => listT.toArray, arrayT => arrayT.toList)

  implicit def optionCodec[T: Codec]: Codec[Option[T]] = new OptionCodec[T]
}
