package co.topl.blockchain

import co.topl.brambl.models.box.Value.UpdateProposal
import co.topl.config.ApplicationConfig
import scala.concurrent.duration._

object PublicTestnet {

  val DefaultProtocol: ApplicationConfig.Bifrost.Protocol =
    ApplicationConfig.Bifrost.Protocol(
      minAppVersion = "2.0.0",
      fEffective = co.topl.models.utility.Ratio(15, 100),
      vrfLddCutoff = 50,
      vrfPrecision = 40,
      vrfBaselineDifficulty = co.topl.models.utility.Ratio(1, 20),
      vrfAmplitude = co.topl.models.utility.Ratio(1, 2),
      // 10x private testnet default, resulting in ~50 minute epochs
      chainSelectionKLookback = 500,
      slotDuration = 1.seconds,
      forwardBiasedSlotWindow = 50,
      operationalPeriodsPerEpoch = 24,
      kesKeyHours = 9,
      kesKeyMinutes = 9
    )

  val DefaultUpdateProposal: UpdateProposal =
    BigBang.protocolToUpdateProposal(DefaultProtocol)

}
