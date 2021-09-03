package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox, ProgramId}
import co.topl.modifier.transaction.builder.BoxPickingStrategy.implicits._
import co.topl.modifier.transaction.builder.ValueTransferBlueprint.implicits._
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

package object builder {

  case class TokenBoxes(
    arbits: IndexedSeq[(Address, ArbitBox)],
    polys:  IndexedSeq[(Address, PolyBox)],
    assets: IndexedSeq[(Address, AssetBox)]
  )

  def buildTransfer[Value, Failure, Transfer, Strategy: BoxPickingStrategy](
    senders:                    IndexedSeq[Address],
    recipients:                 IndexedSeq[(Address, Value)],
    boxReader:                  BoxReader[ProgramId, Address],
    feeChangeAddress:           Address,
    consolidationAddress:       Address,
    fee:                        Int128,
    data:                       Option[Latin1Data] = None,
    minting:                    Boolean = false,
    strategy:                   Strategy = BoxPickingStrategy.All
  )(implicit transferBlueprint: ValueTransferBlueprint[Value, Failure, Transfer]): Either[Failure, Transfer] = {
    val userBoxes = senders.flatMap(addr => boxReader.getTokenBoxes(addr).getOrElse(List()).map(addr -> _))

    val tokenSpecificBoxes = userBoxes.foldLeft(TokenBoxes(IndexedSeq(), IndexedSeq(), IndexedSeq())) {
      case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = boxes.polys :+ (addr -> box))
      case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = boxes.arbits :+ (addr -> box))
      case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = boxes.assets :+ (addr -> box))
      case (boxes, _)                     => boxes
    }

    val pickedBoxes = strategy.pick(tokenSpecificBoxes)

    transferBlueprint.compileBlueprint(
      pickedBoxes,
      recipients,
      feeChangeAddress,
      consolidationAddress,
      fee,
      data,
      minting
    )
  }

  trait Implicits extends BoxPickingStrategy.Implicits with ValueTransferBlueprint.Implicits

  object implicits extends Implicits

}
