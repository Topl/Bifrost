package co.topl.network.codecs.scodecs.settings

import co.topl.settings.Version
import scodec.Codec
import co.topl.codecs._
import shapeless.{::, HNil}

trait SettingsCodecs {

  implicit val versionCodec: Codec[Version] =
    (byteCodec :: byteCodec :: byteCodec).xmapc { case first :: second :: third :: HNil =>
      new Version(first, second, third)
    }(version => version.firstDigit :: version.secondDigit :: version.thirdDigit :: HNil)
}
