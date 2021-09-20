package co.topl.attestation.keyManagement.stakingKeys

import co.topl.crypto.kes.KesVerifier
import co.topl.crypto.kes.keys.SymmetricKey
import co.topl.utils.encode.Base58
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.{Files, Path}
import java.security.SecureRandom
import java.util.Comparator
import scala.util.{Failure, Success, Try}

class HdKesKeySpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  protected var keyFileDir: Path = _

  def beforeAll(): Unit =
    keyFileDir = Files.createTempDirectory("bifrost-test-keyring")

  def afterAll(): Unit = {
    import scala.jdk.CollectionConverters._
    Files
      .walk(keyFileDir)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }

  property("HD KES Test") {
    beforeAll()
    val passed = Try {
      val password = "password"
      val rnd: SecureRandom = new SecureRandom()
      val totalNumberOfKeys = 12
      val registrationSlot = 3000000L
      var hdKesScheme = HdKesScheme(totalNumberOfKeys, registrationSlot)
      var hdKesKeyFile = HdKesKeyFile.newKeyFile(password, keyFileDir.toString, hdKesScheme)
      for (i <- 0 until totalNumberOfKeys) {
        var t = registrationSlot + i * SymmetricKey.maxKeyTimeSteps
        val newKey = hdKesScheme.generateKESKey(i)
        hdKesKeyFile = HdKesKeyFile.updateKeyFile(hdKesKeyFile, hdKesScheme, password, keyFileDir.toString).get
        var productKeyFile = SymmetricKeyFile.newKeyFile(password, keyFileDir.toString, newKey)
        val message = rnd.generateSeed(2048)
        var prodKey = productKeyFile.getKey(password).get
        t += 1
        prodKey = prodKey.update(t)
        productKeyFile = SymmetricKeyFile.updateKeyFile(productKeyFile, prodKey, password, keyFileDir.toString).get
        t += 10
        prodKey = prodKey.update(t)
        var sigProd = prodKey.sign(message)
        assert(KesVerifier.verify(message, sigProd, t))
        t += 100
        prodKey = prodKey.update(t)
        productKeyFile = SymmetricKeyFile.updateKeyFile(productKeyFile, prodKey, password, keyFileDir.toString).get
        sigProd = prodKey.sign(message)
        assert(KesVerifier.verify(message, sigProd, t))
        t += 1000
        prodKey = prodKey.update(t)
        productKeyFile = SymmetricKeyFile.updateKeyFile(productKeyFile, prodKey, password, keyFileDir.toString).get
        t += 10000
        prodKey = prodKey.update(t)
        productKeyFile = SymmetricKeyFile.updateKeyFile(productKeyFile, prodKey, password, keyFileDir.toString).get
        t += 100000
        prodKey = prodKey.update(t)
        productKeyFile = SymmetricKeyFile.updateKeyFile(productKeyFile, prodKey, password, keyFileDir.toString).get
        prodKey = productKeyFile.getKey(password).get
        sigProd = prodKey.sign(message)
        assert(KesVerifier.verify(message, sigProd, t))
        //validation step for index signature
        hdKesScheme = hdKesKeyFile.getKey(password).get
        val vk_i = hdKesScheme.deriveVerificationKey(i).toPublicKey
        require(
          KesVerifier.verify(vk_i, prodKey.getVerificationKey, prodKey.data.offset, prodKey.signature),
          "verification failed"
        )
      }

    } match {
      case Success(_) =>
        true
      case Failure(e) =>
        e.printStackTrace()
        false
    }
    afterAll()
    passed shouldBe true
  }
}
