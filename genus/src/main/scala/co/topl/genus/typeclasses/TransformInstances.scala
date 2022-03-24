package co.topl.genus.typeclasses

import cats.implicits._
import co.topl.genus.types._
import co.topl.utils.mongodb.models._

trait TransformInstances {

  implicit val pairToAttestation: Transform[(String, String), Attestation] =
    pair => Attestation(pair._1, pair._2)

  implicit val tokenValueDataModelToValueType: Transform[TokenValueDataModel, TokenValue] = {
    case SimpleValueDataModel(quantity) =>
      TokenValue(TokenValue.Value.Simple(SimpleValue(quantity)))
    case AssetValueDataModel(code, quantity, securityRoot, metadata) =>
      TokenValue(
        TokenValue.Value.Asset(
          AssetValue(
            code,
            quantity,
            securityRoot,
            metadata.getOrElse("")
          )
        )
      )
  }

  implicit val dataModelToTokenBox: Transform[TokenBoxDataModel, TokenBox] =
    model =>
      TokenBox(
        model.`type`,
        model.id,
        model.nonce,
        model.evidence,
        tokenValueDataModelToValueType.transformTo(model.value).some
      )

  implicit val pairToInputBox: Transform[(String, String), InputBox] =
    pair => InputBox(pair._1, pair._2)

  implicit val pairToOutputBox: Transform[(String, TokenValueDataModel), OutputBox] =
    pair =>
      OutputBox(
        pair._1,
        tokenValueDataModelToValueType.transformTo(pair._2).some
      )

  implicit val txDataToTx: Transform[ConfirmedTransactionDataModel, Transaction] =
    model =>
      Transaction(
        model.txType,
        model.timestamp,
        model.signatures.map(pairToAttestation.transformTo).toList,
        model.newBoxes.map(dataModelToTokenBox.transformTo),
        model.data.getOrElse(""),
        model.from.map(pairToInputBox.transformTo),
        model.minting,
        model.txId,
        model.boxesToRemove,
        model.fee,
        model.to.map(pairToOutputBox.transformTo),
        model.propositionType,
        model.block.id,
        model.block.height
      )

  implicit val blockDataToBlock: Transform[BlockDataModel, Block] =
    model =>
      Block(
        model.id,
        model.parentId,
        model.timestamp,
        dataModelToTokenBox.transformTo(model.generatorBox).some,
        model.publicKey,
        model.signature,
        model.height,
        model.difficulty,
        model.txRoot,
        model.bloomFilter,
        model.version,
        model.numTransactions
      )
}
