package co.topl.models

import co.topl.models.utility.{Lengths, Sized}

/**
 * blockHeader.messageToSign
 */
case class KesCertificate(
  vkKES:      PublicKeys.Kes,
  kesProof:   Sized.Strict[Bytes, Lengths.`64`.type],
  blockProof: Sized.Strict[Bytes, Lengths.`1440`.type],
  slotOffset: Long
)
