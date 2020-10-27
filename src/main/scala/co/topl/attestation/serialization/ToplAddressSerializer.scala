package co.topl.attestation.serialization

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.ToplAddress.{ AddressContent, AddressTypePrefix }
import co.topl.attestation.PublicKeyHashAddress
import co.topl.attestation.evidence.Evidence.{ EvidenceContent, EvidenceTypePrefix }
import co.topl.attestation.evidence.EvidenceEncoder.NetworkPrefix
import co.topl.attestation.evidence.{ Evidence, EvidenceEncoder, PublicKeyAddress, PublicKeyHashAddress }
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.secrets.PrivateKey25519
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ToplAddressSerializer extends BifrostSerializer[Evidence[_ <: Proposition]] {

  def serialize( obj: Evidence[_], w: Writer): Unit = {
    /* networkType: Byte */
    w.put(obj.evidenceEncoder.networkPrefix)

    /* addressType: Byte */
    w.put(obj.typePrefix)

    /* addressBytes: Array[Byte] */
    w.putBytes(obj.content)
  }

  def parse(r: Reader): Evidence[_ <: Proposition] = {
    val networkType: NetworkPrefix = r.getByte()
    val evidenceType: EvidenceTypePrefix = r.getByte()
    val content: EvidenceContent = EvidenceContent @@ r.getBytes(Evidence.contentLength)

    implicit val evidenceEncoder: EvidenceEncoder = new EvidenceEncoder(networkType)
    evidenceType match {
      case prefix @ PublicKeyAddress.addressTypePrefixCurve25519     => new PublicKeyAddress[PrivateKey25519](prefix, EvidenceContent @@ content)
      case prefix @ PublicKeyHashAddress.addressTypePrefixCurve25519 => new PublicKeyHashAddress[PrivateKey25519](prefix, EvidenceContent @@ content)
      case _ => throw new Exception("Invalid address: Unsupported address type " + evidenceType)
    }
  }
}