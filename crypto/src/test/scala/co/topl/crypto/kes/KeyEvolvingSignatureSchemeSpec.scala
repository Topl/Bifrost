package co.topl.crypto.kes

import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.util.{Try,Success,Failure}
import java.security.SecureRandom

class KeyEvolvingSignatureSchemeSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("Sum Composition Keypair Evolution Test") {
    val passed = Try {
      val rnd: SecureRandom = new SecureRandom()
      val kes:KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
      var t = 0
      val logl = 7
      val l = scala.math.pow(2, logl).toInt
      println(l.toString + " time steps")
      t = 0
      val seed1 = rnd.generateSeed(32)
      val seed2 = rnd.generateSeed(32)
      val sk = kes.sumGenerateKey(seed1, logl)
      val pk = kes.sumGetPublicKey(sk)
      val sk2 = kes.sumGenerateKey(seed2, logl)
      val pk2 = kes.sumGetPublicKey(sk2)
      println("tree: " + sk)
      println("Target Private Key Length:")
      println(kes.skBytes * logl + 2 * kes.pkBytes + 3 * kes.hashBytes * logl)
      println("Tree Byte Length:")
      var data: Array[Byte] = Array()
      for (item <- sk.toSeq) {
        data = data ++ item
      }
      println(data.length)
      println("Private Key Time Step:")
      println(kes.sumGetKeyTimeStep(sk))
      println("Verifying key pair")
      require(kes.sumVerifyKeyPair(sk, pk),"Keypair did not validate")
      println("Private Key Update:")
      t += (l * 3) / 4 + 1
      val sk_evolved = kes.sumUpdate(sk, t)
      println("Key t: " + kes.sumGetKeyTimeStep(sk_evolved).toString)
      println("t: " + t.toString)
      println("Tree height: " + sk.height.toString)
      println("Verifying key pair")
      require(pk sameElements kes.sumGetPublicKey(sk_evolved),"Evolved Key did not produce the same public key as original")
      require(kes.sumVerifyKeyPair(sk_evolved, pk),"Evolved Key did not produce the same public key as original")
      require(!kes.sumVerifyKeyPair(sk2, pk),"Invalid keypair verified sumVerifyKeyPair")
      require(!kes.sumVerifyKeyPair(sk, pk2),"Invalid keypair verified sumVerifyKeyPair")
      require(kes.sumVerifyKeyPair(sk2, pk2),"Valid Key pair failed to validate sumVerifyKeyPair")
    } match {
      case Success(_) =>
        true
      case Failure(e) =>
        e.printStackTrace()
        false
    }
    passed shouldBe true
  }

  property("Sum Composition Signature Test") {
    val passed = Try {
      val rnd: SecureRandom = new SecureRandom()
      val kes:KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
      var t = 0
      val message = rnd.generateSeed(2048)
      val logl = 7
      val l = scala.math.pow(2, logl).toInt
      println(l.toString + " time steps")
      val seed1 = rnd.generateSeed(32)
      val seed2 = rnd.generateSeed(32)
      var sk1 = kes.sumGenerateKey(seed1, logl)
      val pk1 = kes.sumGetPublicKey(sk1)
      var sk2 = kes.sumGenerateKey(seed2, logl)
      val pk2 = kes.sumGetPublicKey(sk2)
      t = (l * 1) / 4 + 1
      sk1 = kes.sumUpdate(sk1, t)
      sk2 = kes.sumUpdate(sk2, t)
      val sig11 = kes.sumSign(sk1, message, t)
      val sig21 = kes.sumSign(sk2, message, t)
      require(kes.sumVerify(pk1, message, sig11),"Signature 11 did not verify")
      require(kes.sumVerify(pk2, message, sig21),"Signature 21 did not verify")
      t = (l * 3) / 4 + 1
      sk1 = kes.sumUpdate(sk1, t)
      sk2 = kes.sumUpdate(sk2, t)
      val sig12 = kes.sumSign(sk1, message, t)
      val sig22 = kes.sumSign(sk2, message, t)
      require(kes.sumVerify(pk1, message, sig12),"Signature 12 did not verify")
      require(kes.sumVerify(pk2, message, sig22),"Signature 22 did not verify")
    } match {
      case Success(_) =>
        true
      case Failure(e) =>
        e.printStackTrace()
        false
    }
    passed shouldBe true
  }

  property("Product Composition Test") {
    val passed = Try {
      Console.println("Testing MMM product composition")
      val rnd: SecureRandom = new SecureRandom()
      val kes:KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
      var t = 0
      val message = rnd.generateSeed(2048)
      val seed1 = rnd.generateSeed(32)
      var prodKey = kes.generateKey(seed1)
      val prodPk = kes.publicKey(prodKey)
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("Updating MMM product key")
      prodKey = kes.updateKey(prodKey, t)
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("t: " + t.toString)
      t += 10
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("Updating MMM product key")
      prodKey = kes.updateKey(prodKey, t)
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("t: " + t.toString)
      println("product sign")
      var sigProd = kes.sign(prodKey, message)
      println("product verify")
      assert(kes.verify(prodPk, message, sigProd,t))

      t += 100
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("Updating MMM product key")
      prodKey = kes.updateKeyFast(prodKey, t)

      sigProd = kes.sign(prodKey, message)
      println("product verify")
      assert(kes.verify(prodPk, message, sigProd,t))

      t += 2000
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("Updating MMM product key")
      prodKey = kes.updateKeyFast(prodKey, t)

      t += 10400
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))
      println("Updating MMM product key")
      prodKey = kes.updateKeyFast(prodKey, t)
      t += 1000000
      println("Updating MMM product key")
      prodKey = kes.updateKeyFast(prodKey, t)
      println("Product key time step:")
      println(kes.getKeyTimeStep(prodKey))

      sigProd = kes.sign(prodKey, message)
      println("product verify")
      assert(kes.verify(prodPk, message, sigProd,t))
      println("Product key time step: " + kes.getKeyTimeStep(prodKey).toString)
      println("t: " + t.toString)

      var data:Array[Byte] = Array()
      for (item <- prodKey._1.toSeq) {
        data = data ++ item
      }
      for (item <- prodKey._2.toSeq) {
        data = data ++ item
      }
      data = data ++ prodKey._3 ++ prodKey._4 ++ prodKey._5
      println("Key byte legnth: " + data.length.toString)
      println("Signature byte legnth: " + (sigProd._1++sigProd._2++sigProd._3).length.toString)
    } match {
      case Success(_) =>
        true
      case Failure(e) =>
        e.printStackTrace()
        false
    }
    passed shouldBe true
  }

}
