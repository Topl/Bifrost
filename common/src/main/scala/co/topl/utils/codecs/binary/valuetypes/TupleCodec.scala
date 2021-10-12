package co.topl.utils.codecs.binary.valuetypes

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}

object TupleCodec {

  def encode[A: Encoder, B: Encoder](tuple: (A, B)): Attempt[BitVector] =
    for {
      aBits <- Encoder[A].encode(tuple._1)
      bBits <- Encoder[B].encode(tuple._2)
    } yield aBits ++ bBits

  def encode[A: Encoder, B: Encoder, C: Encoder](tuple: (A, B, C)): Attempt[BitVector] =
    for {
      aBits <- Encoder[A].encode(tuple._1)
      bBits <- Encoder[B].encode(tuple._2)
      cBits <- Encoder[C].encode(tuple._3)
    } yield aBits ++ bBits ++ cBits

  def decode[A: Decoder, B: Decoder](from: BitVector): Attempt[DecodeResult[(A, B)]] =
    for {
      aResult <- Decoder[A].decode(from)
      aResultRemaining = aResult.remainder
      aResultValue = aResult.value
      bResult <- Decoder[B].decode(aResultRemaining)
      bResultRemaining = bResult.remainder
      bResultValue = bResult.value
    } yield DecodeResult((aResultValue, bResultValue), bResultRemaining)

  def decode[A: Decoder, B: Decoder, C: Decoder](from: BitVector): Attempt[DecodeResult[(A, B, C)]] =
    for {
      aResult <- Decoder[A].decode(from)
      aResultRemaining = aResult.remainder
      aResultValue = aResult.value
      bResult <- Decoder[B].decode(aResultRemaining)
      bResultRemaining = bResult.remainder
      bResultValue = bResult.value
      cResult <- Decoder[C].decode(bResultRemaining)
      cResultRemaining = cResult.remainder
      cResultValue = cResult.value
    } yield DecodeResult((aResultValue, bResultValue, cResultValue), cResultRemaining)

  def codec[A: Codec, B: Codec]: Codec[(A, B)] = new Codec[(A, B)] {
    override def encode(value: (A, B)): Attempt[BitVector] = TupleCodec.encode(value)

    override def sizeBound: SizeBound = Codec[A].sizeBound + Codec[B].sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[(A, B)]] = TupleCodec.decode(bits)
  }

  def codec[A: Codec, B: Codec, C: Codec]: Codec[(A, B, C)] = new Codec[(A, B, C)] {
    override def encode(value: (A, B, C)): Attempt[BitVector] = TupleCodec.encode(value)

    override def sizeBound: SizeBound = Codec[A].sizeBound + Codec[B].sizeBound + Codec[C].sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[(A, B, C)]] = TupleCodec.decode(bits)
  }

  trait Codecs {
    def tuple[A: Codec, B: Codec]: Codec[(A, B)] = codec[A, B]

    def tuple[A: Codec, B: Codec, C: Codec]: Codec[(A, B, C)] = codec[A, B, C]
  }

  trait Implicits {
    implicit def implicitTuple[A: Codec, B: Codec]: Codec[(A, B)] = codec[A, B]

    implicit def implicitTuple[A: Codec, B: Codec, C: Codec]: Codec[(A, B, C)] = codec[A, B, C]
  }

  object codecs extends Codecs
  object implicits extends Codecs
}
