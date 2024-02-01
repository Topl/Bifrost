package co.topl.ledger.models

import co.topl.brambl.models.{GroupId, SeriesId}
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.models.Bytes

case class RewardQuantities(lvl: BigInt = BigInt(0), topl: BigInt = BigInt(0), assets: Map[AssetId, BigInt] = Map.empty)

case class AssetId(
  groupId:            Option[GroupId],
  seriesId:           Option[SeriesId],
  groupAlloy:         Option[Bytes],
  seriesAlloy:        Option[Bytes],
  fungibilityType:    FungibilityType,
  quantityDescriptor: QuantityDescriptorType
)
