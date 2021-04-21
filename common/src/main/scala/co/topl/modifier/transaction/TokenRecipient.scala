package co.topl.modifier.transaction

import co.topl.attestation.Address
import co.topl.modifier.box.{TokenBox, TokenValueHolder}

case class TokenRecipient[T <: TokenValueHolder, BX <: TokenBox[T]](
  address:   Address,
  outputBox: TokenBoxOutput[T, BX],
  value:     TokenValueHolder
)
