package co.topl.attestation.keyManagement.stakingKeys

import co.topl.crypto.kes.{KesVerifier, keys}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.{Files, Path}
import java.security.SecureRandom
import java.util.Comparator
import scala.util.{Failure, Success, Try}

class ProductKeyFileSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  protected var keyFileDir: Path = _

  def beforeAll(): Unit = {
    keyFileDir = Files.createTempDirectory("bifrost-test-keyring")
  }

  def afterAll(): Unit = {
    import scala.jdk.CollectionConverters._
    Files
      .walk(keyFileDir)
      .sorted(Comparator.reverseOrder[Path]())
      .iterator()
      .asScala
      .foreach(Files.delete)
  }

  property("ProductKeyFile Test") {
    beforeAll()
    val passed = Try {
      val password = "password"
      val rnd:SecureRandom = new SecureRandom()
      var productKeyFile:ProductKeyFile = ProductKeyFile.newRandomKeyFile(password,keyFileDir.toString,0)
      var prodKey = productKeyFile.getKey(password).get
      var t = 0
      val message = rnd.generateSeed(2048)

      t += 1
      prodKey = prodKey.update(t)
      productKeyFile = ProductKeyFile.updateKeyFile(productKeyFile,prodKey,password,keyFileDir.toString).get
      t += 10
      prodKey = prodKey.update(t)
      var sigProd = prodKey.sign(message)
      assert(KesVerifier.verify(message, sigProd,t))

      t += 100
      prodKey = prodKey.update(t)
      productKeyFile = ProductKeyFile.updateKeyFile(productKeyFile,prodKey,password,keyFileDir.toString).get
      sigProd = prodKey.sign(message)
      assert(KesVerifier.verify(message, sigProd,t))
      t += 1000
      prodKey = prodKey.update(t)
      productKeyFile = ProductKeyFile.updateKeyFile(productKeyFile,prodKey,password,keyFileDir.toString).get
      t += 10000
      prodKey = prodKey.update(t)
      productKeyFile = ProductKeyFile.updateKeyFile(productKeyFile,prodKey,password,keyFileDir.toString).get
      t += 100000
      prodKey = prodKey.update(t)
      productKeyFile = ProductKeyFile.updateKeyFile(productKeyFile,prodKey,password,keyFileDir.toString).get
      prodKey = productKeyFile.getKey(password).get
      sigProd = prodKey.sign(message)
      assert(KesVerifier.verify(message, sigProd,t))

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
