package co.topl.attestation.evidence

import co.topl.attestation.evidence.EvidenceEncoder.NetworkPrefix
import co.topl.attestation.proposition.Proposition
import co.topl.attestation.serialization.ToplAddressSerializer
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import scala.util.Try

class EvidenceEncoder ( val networkPrefix: NetworkPrefix) {

  import EvidenceEncoder._

  implicit val evidenceEncoder: EvidenceEncoder = this

  private def genChecksum(netAddressBytes: Array[Byte]): Array[Byte] = Blake2b256(netAddressBytes).take(checksumLength)

  def toString(ev: Evidence[_]): String = {
    val addressBytes = ev.bytes
    val checksum = genChecksum(addressBytes)
    Base58.encode(addressBytes ++ checksum)
  }

  def fromString(evStr: String): Try[Evidence[_ <: Proposition]] = Base58.decode(evStr).flatMap { bytes =>
    require(bytes.length == encodeEvidenceLength, s"Invalid address: Not the required length")
    require(bytes.head == networkPrefix, s"Invalid address: Not applicable for the current network")
    require(genChecksum(bytes.take(encodeEvidenceLength - checksumLength)) sameElements bytes.takeRight(checksumLength),
            s"Invalid address: Checksum fails for $evStr")

    ToplAddressSerializer.parseBytes(bytes)
  }
}

object EvidenceEncoder {
  type NetworkPrefix = Byte
  val mainNetPrefix: NetworkPrefix = 1.toByte
  val testNetPrefix: NetworkPrefix = 16.toByte
  val devNetPrefix: NetworkPrefix = 32.toByte
  val localNetPrefix: NetworkPrefix = 48.toByte
  val privateNetPrefix: NetworkPrefix = 64.toByte

  val checksumLength = 4

  val encodeEvidenceLength: Int = 38 //addresses are 38 bytes (1 for network prefix, 1 for type prefix, 32 for content, 4 for checksum)
}
