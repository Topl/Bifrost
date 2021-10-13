package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.StringDataTypes.Latin1Data
import co.topl.utils.codecs.{AsBytes, FromBytes}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, Decoder, Encoder, Err, Transform, Transformer}

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
  }

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
      with StaticArrayCodec.Codecs
      with TupleCodec.Codecs
      with UIntCodec.Codecs
      with ULongCodec.Codecs {

    implicit val latin1DataCodec: Codec[Latin1Data] =
      ByteStringCodec.codec.exmapc(byteString =>
        Latin1Data
          .validated(byteString)
          .map(data => Attempt.successful(data))
          .valueOr(errs => Attempt.failure(Err(errs.toString)))
      )(latin1Data => Attempt.successful(new String(latin1Data.value, stringCharacterSet)))
  }

  trait Implicits extends Instances with Codecs

  object codecs extends Codecs
  object implicits extends Implicits
}
