package co.topl.minting.models

import co.topl.models.Slot
import co.topl.consensus.models.EligibilityCertificate
import co.topl.models.utility.Ratio

case class VrfHit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)
