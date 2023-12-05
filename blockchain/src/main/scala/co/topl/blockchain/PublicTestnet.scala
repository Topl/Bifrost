package co.topl.blockchain

import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.config.ApplicationConfig
import scala.concurrent.duration._

object PublicTestnet {

  val DefaultProtocol: ApplicationConfig.Bifrost.Protocol =
    ApplicationConfig.Bifrost.Protocol(
      minAppVersion = "2.0.0",
      fEffective = co.topl.models.utility.Ratio(12997, 100000),
      vrfLddCutoff = 15,
      vrfPrecision = 40,
      vrfBaselineDifficulty = co.topl.models.utility.Ratio(5, 100),
      vrfAmplitude = co.topl.models.utility.Ratio(50, 100),
      // 20x private testnet default, resulting in ~100 minute epochs
      chainSelectionKLookback = 1000,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 24,
      kesKeyHours = 9,
      kesKeyMinutes = 9
    )

  val DefaultUpdateProposal: UpdateProposal =
    BigBang.protocolToUpdateProposal(DefaultProtocol)

}
