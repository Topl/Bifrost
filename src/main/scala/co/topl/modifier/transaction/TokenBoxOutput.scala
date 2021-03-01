package co.topl.modifier.transaction

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.Nonce
import co.topl.modifier.box._

sealed abstract class TokenBoxOutput[T <: TokenValueHolder, BX <: TokenBox[T]] {
  def generateBox: (Evidence, Box.Nonce, T) => BX
}

case object ArbitBoxOutput extends TokenBoxOutput[SimpleValue, ArbitBox] {
  override def generateBox: (Evidence, Nonce, SimpleValue) => ArbitBox = ArbitBox.apply
}

case object PolyBoxOutput extends TokenBoxOutput[SimpleValue, PolyBox] {
  override def generateBox: (Evidence, Nonce, SimpleValue) => PolyBox = PolyBox.apply
}

case object AssetBoxOutput extends TokenBoxOutput[AssetValue, AssetBox] {
  override def generateBox: (Evidence, Nonce, AssetValue) => AssetBox = AssetBox.apply
}
