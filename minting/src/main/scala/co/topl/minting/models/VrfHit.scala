package co.topl.minting.models

import co.topl.models.{EligibilityCertificate, Slot}
import co.topl.models.utility.Ratio

case class VrfHit(cert: EligibilityCertificate, slot: Slot, threshold: Ratio)
