package co.topl.utils.catsInstances.eqs

import cats.implicits._
import cats.Eq
import co.topl.attestation.{Address, Evidence}

trait EqInstances {

  implicit val evidenceEq: Eq[Evidence] = (e1, e2) => e1.evBytes sameElements e2.evBytes

  implicit val addressEq: Eq[Address] = (a1, a2) => a1.evidence === a2.evidence && a1.networkPrefix === a2.networkPrefix
}
