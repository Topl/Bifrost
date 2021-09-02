package co.topl.typeclasses

import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.crypto.hash.blake2b256
import co.topl.models.utility.HasLength.implicits._
import co.topl.models.utility.{Lengths, Ratio, Sized}
import co.topl.models.{Bytes, Evidence, IdentifierTypes, TypedBytes}
import simulacrum.{op, typeclass}

@typeclass trait ContainsEvidence[T] {
  @op("evidence") def evidenceOf(t: T): Evidence
}

object ContainsEvidence {

  trait Instances {

    implicit val ratioContainsEvidence: ContainsEvidence[Ratio] =
      ratio =>
        Sized
          .strict[TypedBytes, Lengths.`33`.type](
            TypedBytes(IdentifierTypes.RatioEvidence, Bytes(blake2b256.hash(ratio.bytes.toArray).value))
          )
          .toOption
          .get
  }
  object Instances extends Instances
}
