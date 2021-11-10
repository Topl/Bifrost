package co.topl.utils.mongodb.models

import co.topl.attestation.Proposition
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer, Transaction}

import java.nio.charset.StandardCharsets
import scala.collection.immutable.ListMap

case class ConfirmedTransactionDataModel(
  block:           BlockSummaryDataModel,
  txType:          String,
  timestamp:       String,
  signatures:      ListMap[String, String],
  newBoxes:        List[TokenBoxDataModel],
  data:            Option[String],
  from:            List[(String, String)],
  minting:         Boolean,
  txId:            String,
  boxesToRemove:   List[String],
  fee:             String,
  to:              List[(String, TokenValueDataModel)],
  propositionType: String
)

object ConfirmedTransactionDataModel {

  def apply(
    blockId:     String,
    blockHeight: Long,
    tx:          Transaction[_, _ <: Proposition]
  ): ConfirmedTransactionDataModel =
    tx match {
      case polyTransfer: PolyTransfer[_] =>
        ConfirmedTransactionDataModel(
          BlockSummaryDataModel(blockId, blockHeight),
          PolyTransfer.typeString,
          polyTransfer.timestamp.toString,
          polyTransfer.attestation.map(pair => pair._1.toString -> pair._2.toString),
          polyTransfer.newBoxes.map(box => TokenBoxDataModel(box)).toList,
          polyTransfer.data.map(data => new String(data.value, StandardCharsets.ISO_8859_1)),
          polyTransfer.from.map(pair => pair._1.toString -> pair._2.toString).toList,
          polyTransfer.minting,
          polyTransfer.id.toString,
          polyTransfer.boxIdsToOpen.map(_.toString).toList,
          polyTransfer.fee.toString,
          polyTransfer.to.map(pair => pair._1.toString -> TokenValueDataModel(pair._2)).toList,
          polyTransfer.getPropIdentifier.typeString
        )
      case arbitTransfer: ArbitTransfer[_] =>
        ConfirmedTransactionDataModel(
          BlockSummaryDataModel(blockId, blockHeight),
          ArbitTransfer.typeString,
          arbitTransfer.timestamp.toString,
          arbitTransfer.attestation.map(pair => pair._1.toString -> pair._2.toString),
          arbitTransfer.newBoxes.map(box => TokenBoxDataModel(box)).toList,
          arbitTransfer.data.map(data => new String(data.value, StandardCharsets.ISO_8859_1)),
          arbitTransfer.from.map(pair => pair._1.toString -> pair._2.toString).toList,
          arbitTransfer.minting,
          arbitTransfer.id.toString,
          arbitTransfer.boxIdsToOpen.map(_.toString).toList,
          arbitTransfer.fee.toString,
          arbitTransfer.to.map(pair => pair._1.toString -> TokenValueDataModel(pair._2)).toList,
          arbitTransfer.getPropIdentifier.typeString
        )
      case assetTransfer: AssetTransfer[_] =>
        ConfirmedTransactionDataModel(
          BlockSummaryDataModel(blockId, blockHeight),
          AssetTransfer.typeString,
          assetTransfer.timestamp.toString,
          assetTransfer.attestation.map(pair => pair._1.toString -> pair._2.toString),
          assetTransfer.newBoxes.map(box => TokenBoxDataModel(box)).toList,
          assetTransfer.data.map(data => new String(data.value, StandardCharsets.ISO_8859_1)),
          assetTransfer.from.map(pair => pair._1.toString -> pair._2.toString).toList,
          assetTransfer.minting,
          assetTransfer.id.toString,
          assetTransfer.boxIdsToOpen.map(_.toString).toList,
          assetTransfer.fee.toString,
          assetTransfer.to.map(pair => pair._1.toString -> TokenValueDataModel(pair._2)).toList,
          assetTransfer.getPropIdentifier.typeString
        )
    }
}
