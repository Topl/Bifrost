package co.topl.typeclasses

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import java.nio.file.{Files, Path}
import java.util.Comparator

class KESCertificateSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  protected var keyFileDir: Path = _

  def beforeAll(): Unit =
    keyFileDir = Files.createTempDirectory("bifrost-test-keyring")

  def afterAll(): Unit =
    Files
      .walk(keyFileDir)
      .sorted(Comparator.reverseOrder[Path]())
      .forEach(p => Files.delete(p))

//  property("ProductKeyFile Test") {
//    beforeAll()
//    val passed = Try {
//      val password = "password"
//      implicit val entropy: Entropy = Entropy.fromUuid(UUID.randomUUID())
//      val signer = KeyInitializer[String => SecretKeys.ExtendedEd25519].random()(password)
//      val rnd: SecureRandom = new SecureRandom()
//
//      val kesKey = SymmetricKey.newFromSeed(
//        rnd.generateSeed(32),
//        0,
//        bytes => Prover[SecretKeys.ExtendedEd25519, Proofs.SignatureEd25519].proveWith(signer, bytes.toArray)
//      )
//      var certFile: SymmetricKeyFile = SymmetricKeyFile.newKeyFile(
//        password,
//        keyFileDir.toString,
//        kesKey
//      )
//      var prodKey = certFile.getKey(password).get
//      var t = 0
//      val message = rnd.generateSeed(2048)
//      t += 1
//      prodKey = prodKey.update(t)
//      certFile = SymmetricKeyFile.updateKeyFile(certFile, prodKey, password, keyFileDir.toString).get
//      t += 10
//      prodKey = prodKey.update(t)
//      var sigProd = prodKey.sign(message)
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//      t += 100
//      prodKey = prodKey.update(t)
//      certFile = SymmetricKeyFile.updateKeyFile(certFile, prodKey, password, keyFileDir.toString).get
//      prodKey = certFile.getKey(password).get
//      sigProd = prodKey.sign(message)
//      assert(OpCertVerifier.verify(message, sigProd, t))
//      t += 1000
//      prodKey = prodKey.update(t)
//      certFile = SymmetricKeyFile.updateKeyFile(certFile, prodKey, password, keyFileDir.toString).get
//      t += 10000
//      prodKey = prodKey.update(t)
//      certFile = SymmetricKeyFile.updateKeyFile(certFile, prodKey, password, keyFileDir.toString).get
//      t += 100000
//      prodKey = prodKey.update(t)
//      certFile = SymmetricKeyFile.updateKeyFile(certFile, prodKey, password, keyFileDir.toString).get
//      prodKey = certFile.getKey(password).get
//      sigProd = prodKey.sign(message)
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    afterAll()
//    passed shouldBe true
//  }
}
