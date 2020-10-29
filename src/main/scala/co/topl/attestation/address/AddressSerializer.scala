package co.topl.attestation.address

import co.topl.attestation.Evidence
import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import co.topl.attestation.address.AddressEncoder.NetworkPrefix
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object AddressSerializer extends BifrostSerializer[Address] {

  def serialize(obj: Address, w: Writer): Unit = {
    /* networkType: Byte */
    w.put(obj.networkPrefix)

    /* addressType: Byte */
    w.put(obj.typePrefix)

    /* addressBytes: Array[Byte] */
    w.putBytes(obj.content)
  }

  def parse(r: Reader): Address = {
    val networkPrefix: NetworkPrefix = r.getByte()
    val evidenceType: EvidenceTypePrefix = r.getByte()
    val content: EvidenceContent = EvidenceContent @@ r.getBytes(Evidence.contentLength)

    Address(networkPrefix, evidenceType, EvidenceContent @@ content)
  }
}