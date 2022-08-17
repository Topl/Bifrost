package co.topl.utils

import co.topl.attestation.keyManagement._
import co.topl.attestation.{PublicKeyPropositionCurve25519, PublicKeyPropositionEd25519, ThresholdPropositionCurve25519}
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.nio.file.{Files, Path}
import java.util.Comparator
import scala.collection.SortedSet

trait KeyRingTestHelper {

  protected val propTypeCurve25519: String = PublicKeyPropositionCurve25519.typeString

  protected val propTypeEd25519: String = PublicKeyPropositionEd25519.typeString

  protected val propTypeThresholdCurve25519: String = ThresholdPropositionCurve25519.typeString

  protected var keyRingCurve25519: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = _

  protected var keyRingEd25519: KeyRing[PrivateKeyEd25519, KeyfileEd25519] = _

  protected var propsThresholdCurve25519: Set[ThresholdPropositionCurve25519] = _
}

trait DiskKeyRingTestHelper extends BeforeAndAfterAll with NetworkPrefixTestHelper with KeyRingTestHelper {

  self: Suite =>

  protected var keyFileDir: Path = _

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

  override def beforeAll(): Unit = {
    super.beforeAll()
    keyRingCurve25519.generateNewKeyPairs(num = 3)
    keyRingEd25519.generateNewKeyPairs(num = 3)
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

trait InMemoryKeyRingTestHelper extends NetworkPrefixTestHelper with BeforeAndAfterAll with KeyRingTestHelper {
  self: Suite =>

  keyRingCurve25519 = {
    implicit def keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
  }

  keyRingEd25519 = {
    implicit def companion: KeyfileEd25519Companion.type = KeyfileEd25519Companion
    KeyRing.empty[PrivateKeyEd25519, KeyfileEd25519]()
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    keyRingCurve25519.generateNewKeyPairs(3)
    keyRingEd25519.generateNewKeyPairs(3)
    propsThresholdCurve25519 = (for (threshold <- 2 to keyRingCurve25519.addresses.size)
      yield ThresholdPropositionCurve25519(
        threshold,
        SortedSet[PublicKeyPropositionCurve25519]() ++ keyRingCurve25519.addresses.flatMap(
          keyRingCurve25519.lookupPublicKey(_).toOption
        )
      )).toSet
  }
}
