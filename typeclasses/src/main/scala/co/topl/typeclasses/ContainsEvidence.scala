package co.topl.typeclasses

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.models.utility.HasLength.instances._
import co.topl.models.utility.{Ratio, Sized}
import co.topl.models.{Bytes, Evidence}
import simulacrum.{op, typeclass}

@typeclass trait ContainsEvidence[T] {
  @op("evidence") def evidenceOf(t: T): Evidence
}

object ContainsEvidence {

  trait Instances {

    implicit val ratioContainsEvidence: ContainsEvidence[Ratio] =
      ratio => Sized.strictUnsafe(Bytes(blake2b256.hash(ratio.bytes.toArray).value))
  }
  object Instances extends Instances
}
