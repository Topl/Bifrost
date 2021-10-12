package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.codecs.{AsBytes, FromBytes}
import scodec.bits.BitVector
import scodec.{Decoder, Encoder, Err, Transform, Transformer}

import java.nio.charset.{Charset, StandardCharsets}
import scala.collection.SortedSet

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
  }

  trait Implicits
      extends Instances
      with BooleanCodec.Implicits
      with ByteCodec.Implicits
      with ByteStringCodec.Implicits
      with valuetypes.Int128Codec.Implicits
      with IntCodec.Implicits
      with IntStringCodec.Implicits
      with ListCodec.Implicits
      with ListMapCodec.Implicits
      with LongCodec.Implicits
      with OptionCodec.Implicits
      with ShortCodec.Implicits
      with TupleCodec.Implicits
      with UIntCodec.Implicits
      with ULongCodec.Implicits

  object implicits extends Implicits

  trait Codecs
      extends BooleanCodec.Codecs
      with ByteCodec.Codecs
      with BytesCodec.Codecs
      with ByteStringCodec.Codecs
      with valuetypes.Int128Codec.Codecs
      with IntCodec.Codecs
      with IntStringCodec.Codecs
      with ListCodec.Codecs
      with ListMapCodec.Codecs
      with LongCodec.Codecs
      with OptionCodec.Codecs
      with ShortCodec.Codecs
      with TupleCodec.Codecs
      with UIntCodec.Codecs
      with ULongCodec.Codecs

  object codecs extends Codecs
}
