package co.topl.utils.codecs.binary.modifier.box

import cats.{Eq, Show}
import co.topl.modifier.box.TokenValueHolder
import co.topl.utils.CommonGenerators
import co.topl.utils.codecs.binary.CodecCompatabilityBehavior
import co.topl.utils.codecs.binary.legacy.modifier.box.TokenValueHolderSerializer
import co.topl.utils.codecs.binary.modifier.box.codecs._
import org.scalacheck.Gen

class TokenValueHolderCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  implicit private val eq: Eq[TokenValueHolder] = Eq.fromUniversalEquals
  implicit private val show: Show[TokenValueHolder] = Show.fromToString

  codecCompatabilityBehavior(
    "token value holder",
    tokenValueHolderCodec,
    TokenValueHolderSerializer,
    Gen.oneOf[TokenValueHolder](simpleValueGen, assetValueGen)
  )
}
