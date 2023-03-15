package co.topl.minting.models

import co.topl.consensus.models._
import co.topl.models.Slot
import com.google.protobuf.ByteString

case class OperationalKeyOut(
  slot:            Slot,
  childVK:         ByteString,
  childSK:         ByteString,
  parentSignature: SignatureKesProduct,
  parentVK:        VerificationKeyKesProduct
)
