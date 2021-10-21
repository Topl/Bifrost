package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Extensions.LongOps
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.binary.ZigZagEncoder.{decodeZigZagInt, decodeZigZagLong, encodeZigZagInt, encodeZigZagLong}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound, Transform, Transformer}

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.SortedSet
import scala.collection.immutable.ListMap
import scala.reflect.ClassTag

package object valuetypes {

  type ULong = Long
  type UInt = Long
  type UShort = Int

  type IntString = String
  type ByteString = String

  /**
   * The number of bits in a byte.
   */
  val byteSize: Int = 8

  val minUIntValue: Long = 0
  val maxUIntValue: Long = 0xffffffffL

  val minUShortValue: Int = 0
  val maxUShortValue: Int = 0xffff

  /**
   * The default character set to use for all `String` encoding/decoding.
   */
  val stringCharacterSet: Charset = StandardCharsets.UTF_8

  trait Instances {

    implicit def decoderToFromBytes[T: Decoder]: FromBytes[Err, T] = bytes =>
      Decoder[T].decode(BitVector(bytes)).toEither.map(_.value).toValidatedNec

    implicit def encoderToAsBytes[T: Encoder]: AsBytes[Err, T] = value =>
      Encoder[T].encode(value).toEither.map(_.toByteArray).toValidatedNec

    implicit def listToSetTransformer[T]: Transformer[List[T], Set[T]] =
      new Transformer[List[T], Set[T]] {

        override def apply[F[_]: Transform](fa: F[List[T]]): F[Set[T]] =
          Transform[F].xmap[List[T], Set[T]](fa, list => list.toSet, set => set.toList)
      }

    implicit def listToSortedSetTransformer[T: Ordering]: Transformer[List[T], SortedSet[T]] =
      new Transformer[List[T], SortedSet[T]] {

        override def apply[F[_]: Transform](fa: F[List[T]]): F[SortedSet[T]] =
          Transform[F].xmap[List[T], SortedSet[T]](fa, listT => SortedSet[T](listT: _*), sortedSet => sortedSet.toList)
      }

    implicit def listToIndexedSeqTransformer[T]: Transformer[List[T], IndexedSeq[T]] =
      new Transformer[List[T], IndexedSeq[T]] {

        override def apply[F[_]: Transform](fa: F[List[T]]): F[IndexedSeq[T]] =
          Transform[F].xmap[List[T], IndexedSeq[T]](fa, listT => listT.toIndexedSeq, seqT => seqT.toList)
      }

    implicit def listToSeqTransformer[T]: Transformer[List[T], Seq[T]] =
      new Transformer[List[T], Seq[T]] {

        override def apply[F[_]: Transform](fa: F[List[T]]): F[Seq[T]] =
          Transform[F].xmap[List[T], Seq[T]](fa, listT => listT.toSeq, seqT => seqT.toList)
      }

    implicit def listToArrayTransformer[T: ClassTag]: Transformer[List[T], Array[T]] =
      new Transformer[List[T], Array[T]] {

        override def apply[F[_]: Transform](fa: F[List[T]]): F[Array[T]] =
          Transform[F].xmap[List[T], Array[T]](fa, listT => listT.toArray, arrayT => arrayT.toList)
      }
  }

  trait Codecs {

    implicit val byteCodec: Codec[Byte] = ByteCodec

    def bytesCodec(size: Int): Codec[Array[Byte]] = new BytesCodec(size)

    implicit val uLongCodec: Codec[ULong] = ULongCodec

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

    implicit val int128Codec: Codec[Int128] =
      bytesCodec(Int128.numBytes).xmap(bytes => Int128(bytes), int => int.toByteArray)

    implicit val byteStringCodec: Codec[ByteString] =
      byteCodec.consume[ByteString](byte =>
        bytesCodec(byte)
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

    implicit def listMapCodec[A: Codec, B: Codec]: Codec[ListMap[A, B]] =
      listCodec[(A, B)].xmap[ListMap[A, B]](list => ListMap(list: _*), listMap => listMap.toList)

    def sizedArrayCodec[T: Codec: ClassTag](size: Int): Codec[Array[T]] =
      sizedListCodec[T](size).xmap[Array[T]](listT => listT.toArray, arrayT => arrayT.toList)

    implicit def optionCodec[T: Codec]: Codec[Option[T]] = new OptionCodec[T]
  }

  object codecs extends Codecs
  object implicits extends Instances
}
