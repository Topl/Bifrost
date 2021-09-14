package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object ULongCodec {

  private val `0x7f` = BitVector(0x7f)
  private val `0x80` = BitVector(0x80)
  private val `0 bitvector` = BitVector(0)

  /**
   * Attempts to decode a `ULong` value from the given vector of bits.
   * This process is outlined more generally at https://developers.google.com/protocol-buffers/docs/encoding#varints
   * @param from the bit vector to attempt a `ULong` decode from
   * @return if the attempt is successful, a `ULong` value and the left-over bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[ULong]] = {
    var result: Long = 0
    var iteration = 0

    while (iteration < 10) {

      // read in the next byte by setting a pointer index
      val bitPointer = iteration * byteSize

      // next byte doesn't exist so insufficient number of bytes error
      if (from.length < bitPointer + byteSize)
        return Attempt.failure(Err.insufficientBits(bitPointer + byteSize, from.length))

      // get the byte reference from the input bit vector
      val b = from.slice(bitPointer, bitPointer + byteSize)

      // update our output long value by replacing the next highest 7 bits with the least-significant
      // 7 bits of the current byte
      result = result | ((b & `0x7f`).toLong() << iteration * 7)

      // return current state of the result long if the current byte & 1000_0000 is
      // equal to 0000_0000
      if ((b & `0x80`) === `0 bitvector`)
        return Attempt.successful(DecodeResult(result, from.drop(bitPointer + byteSize)))

      // otherwise increment the iteration to read in the next byte
      iteration += 1
    }

    Attempt.failure(Err("Unexpected bytes remaining."))
  }

  def encode(value: ULong): Attempt[BitVector] = {
    var output = BitVector.empty
    var runningValue = value

    // loop indefinitely until the process decides its complete
    while (true)
      // last iteration if the most-significant-bit in running value is a 0
      // note: ~0x7f is 1000_0000
      if ((runningValue & ~0x7fL) == 0) {
        output = output ++ BitVector(runningValue.asInstanceOf[Byte])
        return Attempt.successful(output)
      } else {
        // note: 0x7f is 0111_1111 and 0x80 is 1000_0000
        output = output ++ BitVector(((runningValue.asInstanceOf[Int] & 0x7f) | 0x80).toByte)

        // set running value to itself right-shifted by 7
        runningValue >>>= 7
      }

    Attempt.successful(output)
  }

  val codec: Codec[ULong] = new Codec[ULong] {
    override def decode(bits: BitVector): Attempt[DecodeResult[ULong]] = ULongCodec.decode(bits)

    override def encode(value: ULong): Attempt[BitVector] = ULongCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.bounded(8, 64)
  }

  trait Codecs {
    val uLong: Codec[ULong] = codec
  }

  trait Implicits {
    val uLongImplicitCodec: Codec[ULong] = codec
  }

}
