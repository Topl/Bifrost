package co.topl.utils.mongodb.models

import co.topl.modifier.box.{Box, TokenBox, TokenValueHolder}

case class TokenBoxDataModel(boxType: String, id: String, nonce: Long, evidence: String, value: TokenValueDataModel)

object TokenBoxDataModel {

  def apply[T <: TokenValueHolder](tokenBox: TokenBox[T]): TokenBoxDataModel =
    TokenBoxDataModel(
      Box.identifier(tokenBox).typeString,
      tokenBox.id.toString,
      tokenBox.nonce,
      tokenBox.evidence.toString,
      TokenValueDataModel(tokenBox.value)
    )
}
