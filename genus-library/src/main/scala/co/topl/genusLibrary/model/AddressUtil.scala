package co.topl.genusLibrary.model

import co.topl.brambl.models._
import co.topl.brambl.models.transaction.IoTransaction

object AddressUtil {

  def getAddresses(ioTx: IoTransaction): Seq[Address] =
    ioTx.inputs.map(_.address).map(addressFromOutput) ++
    ioTx.outputs.map(_.address).map(addressFromLock)

  private def addressFromOutput(address: TransactionOutputAddress): Address =
    Address(
      address.network,
      address.ledger,
      address.index,
      id = address.id match {
        case TransactionOutputAddress.Id.Empty                   => Identifier(Identifier.Value.Empty)
        case TransactionOutputAddress.Id.IoTransaction32(ioTx32) => Identifier(Identifier.Value.IoTransaction32(ioTx32))
        case TransactionOutputAddress.Id.IoTransaction64(ioTx64) => Identifier(Identifier.Value.IoTransaction64(ioTx64))
      }
    )

  private def addressFromLock(address: LockAddress): Address =
    Address(
      address.network,
      address.ledger,
      index = 0, // TODO, should be None
      id = address.id match {
        case LockAddress.Id.Empty          => Identifier(Identifier.Value.Empty)
        case LockAddress.Id.Lock32(lock32) => Identifier(Identifier.Value.Lock32(lock32))
        case LockAddress.Id.Lock64(lock64) => Identifier(Identifier.Value.Lock64(lock64))
      }
    )
}
