package co.topl.models

import co.topl.models.utility.Ratio

object Vrf {

  case class Hit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)

  case class Config(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)
}
