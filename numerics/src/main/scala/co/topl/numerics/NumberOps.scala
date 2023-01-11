package co.topl.numerics

import co.topl.models.Int128
import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized

import scala.language.implicitConversions

trait NumberOps {

  implicit def intAsInt128(int: Int): Int128 =
    Sized.maxUnsafe(BigInt(int))

  implicit def longAsInt128(long: Long): Int128 =
    Sized.maxUnsafe(BigInt(long))
}
