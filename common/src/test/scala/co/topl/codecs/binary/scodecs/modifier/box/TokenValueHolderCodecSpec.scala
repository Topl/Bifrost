package co.topl.codecs.binary.scodecs.modifier.box

import co.topl.codecs.binary.CodecCompatabilityBehavior
import co.topl.codecs.binary.legacy.modifier.box.TokenValueHolderSerializer
import co.topl.modifier.box.TokenValueHolder
import co.topl.utils.CommonGenerators
import co.topl.utils.implicits._
import org.scalacheck.Gen

class TokenValueHolderCodecSpec extends CodecCompatabilityBehavior with CommonGenerators {

  codecCompatabilityBehavior(
    "token value holder",
    tokenValueHolderCodec,
    TokenValueHolderSerializer,
    Gen.oneOf[TokenValueHolder](simpleValueGen, assetValueGen)
  )
}
