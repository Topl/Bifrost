package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox, ProgramId}
import co.topl.modifier.transaction.builder.BoxPickingStrategy.implicits._
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

package object builder {

  /**
   * Represents a set of available boxes for use in a transaction.
   * @param arbits the available arbit boxes
   * @param polys the available poly boxes
   * @param assets the available asset boxes
   */
  case class TokenBoxes(
    arbits: IndexedSeq[(Address, ArbitBox)],
    polys:  IndexedSeq[(Address, PolyBox)],
    assets: IndexedSeq[(Address, AssetBox)]
  )

  /**
   * Builds a transfer transaction for a token value with a given strategy for choosing input boxes.
   * @param senders a set of addresses that will be sending tokens
   * @param recipients a set of recipients and the amount of tokens they should receive
   * @param boxReader a state reader which provides available boxes for a given address
   * @param feeChangeAddress the address to send fee change to
   * @param consolidationAddress the address to send extra tokens to
   * @param fee the fee to pay for the transaction
   * @param data the metadata text to add to the transaction
   * @param minting whether or not the output tokens should be minted into existence
   * @param strategy the strategy parameters for choosing which input boxes should be selected
   * @param transferBlueprint the blueprint function for creating the transfer transaction from a set of transfer
   *                          parameters
   * @tparam Value the type of token value to send
   * @tparam Failure the type of failure that can occur during the transaction validation
   * @tparam Transfer the output type of created transfer
   * @tparam Strategy the strategy to use for picking transaction input boxes
   * @return
   */
  def buildTransfer[Value, Failure, Transfer, Strategy: BoxPickingStrategy](
    senders:                    IndexedSeq[Address],
    recipients:                 IndexedSeq[(Address, Value)],
    boxReader:                  BoxReader[ProgramId, Address],
    feeChangeAddress:           Address,
    fee:                        Int128,
    consolidationAddress:       Option[Address] = None,
    data:                       Option[Latin1Data] = None,
    minting:                    Boolean = false,
    strategy:                   Strategy = BoxPickingStrategy.All
  )(implicit transferBlueprint: ValueTransferBlueprint[Value, Failure, Transfer]): Either[Failure, Transfer] = {

    // get available boxes to send
    val userBoxes = senders.flatMap(addr => boxReader.getTokenBoxes(addr).getOrElse(List()).map(addr -> _))

    // organize boxes by token type
    val tokenSpecificBoxes = userBoxes.foldLeft(TokenBoxes(IndexedSeq(), IndexedSeq(), IndexedSeq())) {
      case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = boxes.polys :+ (addr -> box))
      case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = boxes.arbits :+ (addr -> box))
      case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = boxes.assets :+ (addr -> box))
      case (boxes, _)                     => boxes
    }

    // filter boxes using the box picking strategy
    val pickedBoxes = strategy.pick(tokenSpecificBoxes)

    // compile a transfer transaction using all the available parameters
    transferBlueprint.compileBlueprint(
      pickedBoxes,
      recipients,
      feeChangeAddress,
      consolidationAddress.getOrElse(feeChangeAddress), // consolidation is same as fee change address by default
      fee,
      data,
      minting
    )
  }

  trait Implicits extends BoxPickingStrategy.Implicits with ValueTransferBlueprint.Implicits

  object implicits extends Implicits

}
