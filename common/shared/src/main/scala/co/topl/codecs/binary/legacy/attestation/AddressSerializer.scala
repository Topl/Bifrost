package co.topl.codecs.binary.legacy.attestation

import co.topl.attestation.{Address, Evidence}
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.utils.NetworkType.NetworkPrefix

object AddressSerializer extends BifrostSerializer[Address] {

  def serialize(obj: Address, w: Writer): Unit = {
    /* networkType: Byte */
    w.put(obj.networkPrefix)

    /* addressBytes: Array[Byte] */
    EvidenceSerializer.serialize(obj.evidence, w)
  }

  def parse(r: Reader): Address = {
    implicit val networkPrefix: NetworkPrefix = r.getByte()
    val evidence: Evidence = EvidenceSerializer.parse(r)
    Address(evidence)
  }
}
