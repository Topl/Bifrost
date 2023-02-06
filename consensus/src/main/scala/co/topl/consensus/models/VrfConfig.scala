package co.topl.consensus.models

import co.topl.models.utility.Ratio

case class VrfConfig(lddCutoff: Int, precision: Int, baselineDifficulty: Ratio, amplitude: Ratio)
