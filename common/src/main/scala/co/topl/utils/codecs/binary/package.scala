package co.topl.utils.codecs

import cats.implicits._
import scodec.bits.BitVector
import scodec.{Decoder, Encoder, Err}

import java.nio.charset.{Charset, StandardCharsets}
import scala.language.implicitConversions

package object binary {

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
  }

  trait Implicits
      extends Instances
      with BooleanCodec.Implicits
      with ByteCodec.Implicits
      with Int128Codec.Implicits
      with IntCodec.Implicits
      with IntStringCodec.Implicits
      with LongCodec.Implicits
      with OptionCodec.Implicits
      with ShortCodec.Implicits
      with ByteStringCodec.Implicits
      with UIntCodec.Implicits
      with ULongCodec.Implicits

  object implicits extends Implicits

  trait Codecs
      extends BooleanCodec.Codecs
      with ByteCodec.Codecs
      with BytesCodec.Codecs
      with Int128Codec.Codecs
      with IntCodec.Codecs
      with IntStringCodec.Codecs
      with LongCodec.Codecs
      with OptionCodec.Codecs
      with ShortCodec.Codecs
      with ByteStringCodec.Codecs
      with UIntCodec.Codecs
      with ULongCodec.Codecs

  object codecs extends Codecs
}
