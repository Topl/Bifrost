package co.topl.utils.mongodb.models

import co.topl.modifier.box.{Box, TokenBox, TokenValueHolder}

case class TokenBoxDataModel(`type`: String, id: String, nonce: String, evidence: String, value: TokenValueDataModel)

object TokenBoxDataModel {

  def apply[T <: TokenValueHolder](tokenBox: TokenBox[T]): TokenBoxDataModel =
    TokenBoxDataModel(
      Box.identifier(tokenBox).typeString,
      tokenBox.id.toString,
      tokenBox.nonce.toString,
      tokenBox.evidence.toString,
      TokenValueDataModel(tokenBox.value)
    )
}
