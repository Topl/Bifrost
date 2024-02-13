package co.topl.ledger.models

import cats.implicits._
import cats.{Monoid, Show}
import co.topl.brambl.models.box.{FungibilityType, QuantityDescriptorType}
import co.topl.brambl.models.{GroupId, SeriesId}
import co.topl.models.Bytes
import co.topl.typeclasses.implicits._

case class RewardQuantities(
  lvl:    BigInt = BigInt(0),
  topl:   BigInt = BigInt(0),
  assets: Map[AssetId, BigInt] = Map.empty
) {

  /**
   * Indicates if this RewardQuantities contains 0-value across all value types.
   * @return true if there are no rewards
   */
  def isEmpty: Boolean =
    lvl <= 0 && topl <= 0 && assets.forall(_._2 <= 0)
}

object RewardQuantities {

  implicit val monoidTransactionRewardQuantities: Monoid[RewardQuantities] = Monoid.instance[RewardQuantities](
    RewardQuantities(),
    (q1, q2) =>
      RewardQuantities(
        q1.lvl + q2.lvl,
        q1.topl + q2.topl,
        (q1.assets.toList ++ q2.assets.toList).groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap
      )
  )

  implicit val showRewardQuantities: Show[RewardQuantities] = { quantities =>
    val quantitiesString = (List(
      "lvl"  -> quantities.lvl,
      "topl" -> quantities.topl
    ) ++ quantities.assets.toList.map { case (id, quantity) => id.show -> quantity })
      .filter(_._2 > 0)
      .map { case (key, value) =>
        s"$key=$value"
      }
      .mkString(", ")
    show"Reward($quantitiesString)"
  }
}

case class AssetId(
  groupId:            Option[GroupId],
  seriesId:           Option[SeriesId],
  groupAlloy:         Option[Bytes],
  seriesAlloy:        Option[Bytes],
  fungibilityType:    FungibilityType,
  quantityDescriptor: QuantityDescriptorType
)

object AssetId {

  implicit val showAssetId: Show[AssetId] =
    assetId =>
      show"${assetId.groupId.map(_.value)}" +
      show":${assetId.seriesId.map(_.value)}" +
      show":${assetId.groupAlloy}" +
      show":${assetId.seriesAlloy}" +
      show":${assetId.fungibilityType.toString}" +
      show":${assetId.quantityDescriptor.toString}"
}
