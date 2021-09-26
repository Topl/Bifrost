package co.topl.modifier.transaction.builder

import co.topl.attestation.Address
import co.topl.modifier.box.AssetValue
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

sealed trait TransferRequest

object TransferRequests {

  case class PolyTransferRequest(
    from:          List[Address],
    to:            List[(Address, Int128)],
    changeAddress: Address,
    fee:           Int128,
    data:          Option[Latin1Data]
  ) extends TransferRequest

  case class ArbitTransferRequest(
    from:                 List[Address],
    to:                   List[(Address, Int128)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data]
  ) extends TransferRequest

  case class AssetTransferRequest(
    from:                 List[Address],
    to:                   List[(Address, AssetValue)],
    changeAddress:        Address,
    consolidationAddress: Address,
    fee:                  Int128,
    data:                 Option[Latin1Data],
    minting:              Boolean
  ) extends TransferRequest
}
