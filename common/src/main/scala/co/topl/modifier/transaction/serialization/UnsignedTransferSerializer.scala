package co.topl.modifier.transaction.serialization

import co.topl.attestation.AddressSerializer
import co.topl.modifier.box.TokenValueHolder
import co.topl.modifier.transaction.unsigned.{PropositionType, TokenValue, UnsignedTransferTransaction}
import co.topl.utils.serialization.{stringCharacterSet, BifrostSerializer, Reader, Writer}

object UnsignedTransferSerializer extends BifrostSerializer[UnsignedTransferTransaction] {

  override def serialize(obj: UnsignedTransferTransaction, w: Writer): Unit = {
    /* Byte */ //this is used to signal the types of propositions in the transactions
    w.put(PropositionType.getTypePrefix(obj.propositionType))

    /* from: IndexedSeq[(Address, Nonce)] */
    w.putUInt(obj.from.length)
    obj.from.foreach { case (addr, nonce) =>
      AddressSerializer.serialize(addr, w)
      w.putLong(nonce)
    }

    /* to: IndexedSeq[(Address, Long)] */
    w.putUInt(obj.to.length)
    obj.to.foreach { case (addr, value) =>
      AddressSerializer.serialize(addr, w)
      TokenValueHolder.serialize(TokenValue.getTokenValueHolder(value), w)
    }

    // empty attestation map
    w.putUInt(0)

    /* fee: Int128 */
    w.putInt128(obj.fee)

    /* timestamp: Long */
    w.putULong(obj.timestamp)

    /* data: Option[String] */
    w.putOption(obj.data) { (writer, d) =>
      writer.putByteString(new String(d.value, stringCharacterSet))
    }

    /* minting: Boolean */
    w.putBoolean(obj.minting)
  }

  override def parse(r: Reader): UnsignedTransferTransaction =
    throw new NotImplementedError("Do not parse bytes into an unsigned transfer transaction.")
}
