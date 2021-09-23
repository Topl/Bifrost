package co.topl.modifier.transaction.unsigned

import co.topl.attestation.{Address, EvidenceProducer, Proof, Proposition}
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.box.{ArbitBox, AssetBox, Box, BoxId, PolyBox, TokenBox, TokenValueHolder}
import co.topl.modifier.transaction.{PolyTransfer, Transaction, TransferTransaction, boxNonceGenerator}
import co.topl.modifier.transaction.serialization.UnsignedTransferSerializer
import co.topl.modifier.transaction.unsigned.TokenValue.getTokenValueHolder
import co.topl.utils.{Identifiable, Int128}
import co.topl.utils.StringDataTypes.Latin1Data

import scala.collection.immutable.ListMap

sealed abstract class UnsignedTokenTransfer[P <: Proposition : Identifiable : EvidenceProducer]

object UnsignedTokenTransfers {
  case class UnsignedPolyTransfer[P <: Proposition : Identifiable : EvidenceProducer](
      from: List[(Address, Box.Nonce)],
      to: List[(Address, Int128)],
      fee: Int128,
      timestamp: Long,
      data: Option[Latin1Data]
                                                                                     ) extends UnsignedTokenTransfer
  case class UnsignedArbitTransfer[P <: Proposition : Identifiable : EvidenceProducer]() extends UnsignedTokenTransfer
  case class UnsignedAssetTransfer[P <: Proposition : Identifiable : EvidenceProducer]() extends UnsignedTokenTransfer
}

case class UnsignedTransferTransaction(
  propositionType: PropositionType,
  transferType:    TokenType,
  from:            List[(Address, Box.Nonce)],
  to:              List[(Address, TokenValue)],
  fee:             Int128,
  timestamp:       Long,
  data:            Option[Latin1Data],
  minting:         Boolean
) {

  val fromBoxIds: List[BoxId] =
    from.map { case (address, nonce) =>
      BoxId.idFromEviNonce(address.evidence, nonce)
    }

  private val getBoxNonce = boxNonceGenerator(TokenType.getTypePrefix(transferType), fromBoxIds, fee, timestamp)

  val toBoxes: List[TokenBox[TokenValueHolder]] =
    to.zipWithIndex.map {
      case ((address, TokenValues.AssetTokens(value)), index) => AssetBox(address.evidence, getBoxNonce(index), value)
      case ((address, TokenValues.PolyTokens(value)), index)  => PolyBox(address.evidence, getBoxNonce(index), value)
      case ((address, TokenValues.ArbitTokens(value)), index) => ArbitBox(address.evidence, getBoxNonce(index), value)
    }

  lazy val messageToSign = UnsignedTransferSerializer.toBytes(this)

  lazy val idBytes: Array[Byte] = Transaction.modifierTypeId.value +: blake2b256.hash(messageToSign).value

  def withSignatures[P <: Proposition : Identifiable : EvidenceProducer](
    signatures: ListMap[P, Proof[P]]
  ): Either[String, TransferTransaction[TokenValueHolder, Proposition]] = {
    Either.cond(
      PropositionType.fromPropositionId(Identifiable[P].getId).get == propositionType,
      {},
      "Invalid signatures proposition type"
    ).map(_ => transferType match {
      case TokenTypes.Polys =>
        PolyTransfer[P](
          from.toIndexedSeq,
          to.map(x => x._1 -> getTokenValueHolder(x._2)),
          signatures,
          fee,
          timestamp,
          data,
          minting
        )
    })

    for {
      _ <-

    } yield "gah"
  }

}
