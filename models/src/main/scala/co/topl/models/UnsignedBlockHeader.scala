package co.topl.models

import co.topl.consensus.models._
import com.google.protobuf.ByteString

case class UnsignedBlockHeader(
  parentHeaderId:                BlockId,
  parentSlot:                    Slot,
  txRoot:                        ByteString,
  bloomFilter:                   ByteString,
  timestamp:                     Timestamp,
  height:                        Long,
  slot:                          Slot,
  eligibilityCertificate:        EligibilityCertificate,
  partialOperationalCertificate: UnsignedBlockHeader.PartialOperationalCertificate,
  metadata:                      ByteString,
  address:                       StakingAddress
)

object UnsignedBlockHeader {

  case class PartialOperationalCertificate(
    parentVK:        VerificationKeyKesProduct,
    parentSignature: SignatureKesProduct,
    childVK:         ByteString
  )

}
