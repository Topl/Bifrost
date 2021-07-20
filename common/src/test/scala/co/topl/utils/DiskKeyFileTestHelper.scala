package co.topl.utils

import co.topl.attestation.keyManagement.{KeyRing, KeyfileCurve25519, KeyfileCurve25519Companion, PrivateKeyCurve25519}
import org.scalatest.{BeforeAndAfterAll, Suite}

import java.nio.file.{Files, Path}
import java.util.Comparator

trait DiskKeyFileTestHelper extends BeforeAndAfterAll with NetworkPrefixTestHelper {

  self: Suite =>

  implicit protected def keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion

  protected var keyFileDir: Path = _

  protected var keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    keyFileDir = Files.createTempDirectory("bifrost-test-keyring")
    keyRing = KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519](Some(keyFileDir.toString))
    import org.scalatest.TryValues._
    keyRing.generateNewKeyPairs(num = 3).success.value
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    import scala.collection.JavaConverters._
    Files
      .walk(keyFileDir)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }
}

trait InMemoryKeyFileTestHelper extends NetworkPrefixTestHelper with BeforeAndAfterAll {
  self: Suite =>

  protected val keyRing: KeyRing[PrivateKeyCurve25519, KeyfileCurve25519] = {
    implicit def keyfileCurve25519Companion: KeyfileCurve25519Companion.type = KeyfileCurve25519Companion
    KeyRing.empty[PrivateKeyCurve25519, KeyfileCurve25519]()
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    keyRing.generateNewKeyPairs(3)
  }
}
