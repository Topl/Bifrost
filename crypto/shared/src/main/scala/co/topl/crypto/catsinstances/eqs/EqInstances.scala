package co.topl.crypto.catsinstances.eqs

import cats.Eq
import co.topl.crypto.hash.digest.implicits._
import co.topl.crypto.hash.digest.Digest

trait EqInstances {
  implicit def digestEq[T: Digest]: Eq[T] = (digestA, digestB) => digestA.bytes.sameElements(digestB.bytes)
}
