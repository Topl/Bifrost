package co.topl.typeclasses

import cats.Eq
import cats.implicits._
import co.topl.models.utility.Sized
import co.topl.models.{Bytes, TypedBytes}

trait EqInstances {

  implicit def arrayEq[T: Eq]: Eq[Array[T]] =
    (a, b) => a.length == b.length && a.zip(b).forall { case (a1, b1) => a1 === b1 }

  implicit val bytesEq: Eq[Bytes] =
    (a, b) => a.toArray === b.toArray

  implicit val typedBytesEq: Eq[TypedBytes] =
    (a, b) => a.allBytes === b.allBytes

  implicit def sizedMaxEq[T: Eq, L]: Eq[Sized.Max[T, L]] =
    (a, b) => a.data === b.data

  implicit def sizedStrictEq[T: Eq, L]: Eq[Sized.Strict[T, L]] =
    (a, b) => a.data === b.data
}
