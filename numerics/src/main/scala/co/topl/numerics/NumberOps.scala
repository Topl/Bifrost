package co.topl.numerics

import com.google.protobuf.ByteString
import quivr.models.Int128

import scala.language.implicitConversions

trait NumberOps {

  implicit def int128AsBigInt(int128: Int128): BigInt =
    BigInt(int128.value.toByteArray)

  implicit def bigIntAsInt128(bigInt: BigInt): Int128 = {
    val arr = bigInt.toByteArray
    if (arr.length > 16) throw new IllegalArgumentException("Int128 cannot be larger than 16 bytes")
    Int128(ByteString.copyFrom(arr))
  }

  implicit def intAsInt128(int: Int): Int128 =
    BigInt(int)

  implicit def longAsInt128(long: Long): Int128 =
    BigInt(long)
}
