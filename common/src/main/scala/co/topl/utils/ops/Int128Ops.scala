package co.topl.utils.ops

import co.topl.models.utility.HasLength.instances.bigIntLength
import co.topl.models.utility.Sized
import co.topl.utils.Int128
import co.topl.models.{Int128 => SizedInt128}

import scala.language.implicitConversions

class Int128Ops(private val int128: Int128) extends AnyVal {

  def toSized: SizedInt128 = Sized.maxUnsafe(int128.bigInt)
}

object Int128Ops {

  trait ToInt128Ops {
    implicit def int128opsFromInt128(int128: Int128): Int128Ops = new Int128Ops(int128)
  }

  trait Implicits extends ToInt128Ops
  object implicits extends Implicits
}
