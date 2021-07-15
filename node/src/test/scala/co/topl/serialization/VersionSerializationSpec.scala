package co.topl.serialization

import co.topl.settings.VersionSerializer
import co.topl.utils.NodeGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class VersionSerializationSpec
    extends AnyPropSpec
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with NodeGenerators {

  property("Version serialization") {
    forAll(versionGen) { version =>
      val parsed = VersionSerializer.parseBytes(VersionSerializer.toBytes(version)).get

      parsed.bytes should contain theSameElementsInOrderAs version.bytes
    }
  }
}
