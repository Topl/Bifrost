package co.topl.utils

import co.topl.attestation.keyManagement._
import co.topl.attestation.{PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519, ThresholdPropositionCurve25519}
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.collection.SortedSet

trait KeyFileTestHelper extends BeforeAndAfterAll with NetworkPrefixTestHelper {

  self: Suite =>

  protected var keyFileDir: Path = _

  protected var keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = _

  protected var keyRingEd25519: KeyRing[PrivateKeyEd25519, KeyfileEd25519] = _

  protected var propsThresholdCurve25519: Set[ThresholdPropositionCurve25519] = _

  protected val propTypeCurve25519: String = PublicKeyPropositionCurve25519.typeString

  protected val propTypeEd25519: String = PublicKeyPropositionEd25519.typeString

  protected val propTypeThresholdCurve25519: String = ThresholdPropositionCurve25519.typeString

  override def beforeAll(): Unit = {
    super.beforeAll()
    keyFileDir = Files.createTempDirectory("bifrost-test-keyring")
    keyRingCurve25519 = KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](Some(keyFileDir.toString))(
      networkPrefix,
      PrivateKeyCurve25519.secretGenerator,
      KeyfileCurve25519Companion
    )
    keyRingEd25519 = KeyRing.empty[PrivateKeyEd25519, KeyfileEd25519](Some(keyFileDir.toString))(
      networkPrefix,
      PrivateKeyEd25519.secretGenerator,
      KeyfileEd25519Companion
    )
    import org.scalatest.TryValues._
    keyRingCurve25519.generateNewKeyPairs(num = 3).success.value
    keyRingEd25519.generateNewKeyPairs(num = 3).success.value

    propsThresholdCurve25519 = (for (threshold <- 2 to keyRingCurve25519.addresses.size)
      yield ThresholdPropositionCurve25519(
        threshold,
        SortedSet[PublicKeyPropositionCurve25519]() ++ keyRingCurve25519.addresses.flatMap(
          keyRingCurve25519.lookupPublicKey(_).toOption
        )
      )).toSet
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    import scala.jdk.CollectionConverters._
    Files
      .walk(keyFileDir)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }
}
