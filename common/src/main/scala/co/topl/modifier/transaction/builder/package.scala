package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.modifier.BoxReader
import co.topl.modifier.box.{ArbitBox, AssetBox, PolyBox, ProgramId}
import co.topl.modifier.transaction.builder.BoxPickingStrategy.implicits._
import co.topl.modifier.transaction.builder.TransferBlueprint.implicits._
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

package object builder {

  case class TokenBoxes(
    arbits: IndexedSeq[(Address, ArbitBox)],
    polys:  IndexedSeq[(Address, PolyBox)],
    assets: IndexedSeq[(Address, AssetBox)]
  )

  def buildTransfer[Blueprint, Failure, Transfer, Strategy: BoxPickingStrategy](
    blueprint:                  Blueprint,
    senders:                    List[Address],
    boxReader:                  BoxReader[ProgramId, Address],
    changeAddress:              Address,
    fee:                        Int128,
    strategy:                   Strategy,
    data:                       Option[Latin1Data] = None
  )(implicit transferBlueprint: TransferBlueprint[Blueprint, Failure, Transfer]): Either[Failure, Transfer] = {
    val userBoxes = senders.flatMap(addr => boxReader.getTokenBoxes(addr).getOrElse(List()).map(addr -> _))

    val tokenSpecificBoxes = userBoxes.foldLeft(TokenBoxes(IndexedSeq(), IndexedSeq(), IndexedSeq())) {
      case (boxes, (addr, box: PolyBox))  => boxes.copy(polys = boxes.polys :+ (addr -> box))
      case (boxes, (addr, box: ArbitBox)) => boxes.copy(arbits = boxes.arbits :+ (addr -> box))
      case (boxes, (addr, box: AssetBox)) => boxes.copy(assets = boxes.assets :+ (addr -> box))
      case (boxes, _)                     => boxes
    }

    val pickedBoxes = strategy.pick(tokenSpecificBoxes)

    blueprint.compileTransfer[Failure, Transfer](pickedBoxes, senders, changeAddress, fee, data)
  }

  trait Implicits
      extends TransferBlueprints.Implicits
      with BoxPickingStrategy.Implicits
      with TransferBlueprint.Implicits

  object implicits extends Implicits

}
