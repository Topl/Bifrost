package co.topl.modifier

import co.topl.attestation.{Address, Evidence}
import co.topl.nodeView.state.box.Box.Nonce
import co.topl.nodeView.state.box._

package object transaction {

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

  case class TokenRecipient[T <: TokenValueHolder, BX <: TokenBox[T]](
    address:   Address,
    outputBox: TokenBoxOutput[T, BX],
    value:     TokenValueHolder
  )
}
