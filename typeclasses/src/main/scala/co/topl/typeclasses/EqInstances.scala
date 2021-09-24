package co.topl.typeclasses

import cats.Eq
import co.topl.models.Evidence

trait EqInstances {
  implicit val evidence: Eq[Evidence]
}
