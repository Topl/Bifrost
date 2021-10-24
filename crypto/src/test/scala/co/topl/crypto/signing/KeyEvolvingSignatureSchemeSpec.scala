//package co.topl.crypto.signing
//
//import co.topl.crypto.kes.keys
//import co.topl.models.Proofs
//import co.topl.models.utility.Sized
//import org.scalatest.matchers.should.Matchers
//import org.scalatest.propspec.AnyPropSpec
//import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
//
//import java.security.SecureRandom
//import scala.util.{Failure, Success, Try}
//
//class KeyEvolvingSignatureSchemeSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {
//
//  property("Sum Composition Key Size and Signature Size") {
//    val passed = Try {
//      val rnd: SecureRandom = new SecureRandom()
//      val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
//      var t = 0
//      val logl = 9
//      val l = scala.math.pow(2, logl).toInt
//      //println(l.toString + " time steps")
//      t = 0
//      val seed1 = rnd.generateSeed(32)
//      val sk = kes.sumCompositionGenerateKey(seed1, logl)
//      val pk = kes.sumCompositionGetPublicKey(sk)
//      //println("tree: " + sk)
//      //println("Target Private Key Length:")
//      //println(kes.skBytes * logl + 2 * kes.pkBytes + 3 * kes.hashBytes * logl)
//      //println("Tree Byte Length:")
//      var data: Array[Byte] = Array()
//      for (item <- sk.toSeq)
//        data = data ++ item
//      //println(data.length)
//      t += (l * 3) / 4 + 1
//      val sk_evolved = kes.sumCompositionUpdate(sk, t)
//      var data2: Array[Byte] = Array()
//      for (item <- sk.toSeq)
//        data2 = data2 ++ item
//      require(data.length == data2.length, "Evolved key length did not match initial length")
//      require(
//        kes.sumCompositionVerifyKeyPair(sk_evolved, pk),
//        "Evolved Key did not produce the same public key as original"
//      )
//      val signature = kes.sumCompositionSign(sk_evolved, Array(), t)
//      //println("Signature Length: "+signature.length.toString)
//      val prodKey = kes.generateAsymmetricProductKey(seed1, 0)
//      val productSignature = kes.signAsymmetricProduct(prodKey, Array())
//      productSignature match {
//        case signatures.AsymmetricSignature(sigi, sigm, pki, offset, pkl) =>
//        //println("Product Sig Length t 0: "+(sigi.length+sigm.length+pki.bytes.length).toString)
//        case _ =>
//      }
//      var oldSigSize = 0
//      var prodKey2 = kes.generateAsymmetricProductKey(seed1, 0)
//      var i = 0
//      while (oldSigSize < 1500) {
//        i += 1
//        prodKey2 = kes.updateAsymmetricProductKey(prodKey2, i)
//        val productSignature2 = kes.signAsymmetricProduct(prodKey2, Array())
//        productSignature2 match {
//          case signatures.AsymmetricSignature(sigi, sigm, pki, offset, pkl) =>
//            val sigSize = sigi.length + sigm.length + pki.value.length
//            if (sigSize > oldSigSize) {
//              oldSigSize = sigSize
//              //println("Product Sig Length t "+i.toString+": "+sigSize.toString)
//              //println(sigi.length,sigm.length,pki.bytes.length)
//            }
//          case _ =>
//        }
//      }
//
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    passed shouldBe true
//  }
//
//  property("Sum Composition Keypair Evolution Test") {
//    val passed = Try {
//      val rnd: SecureRandom = new SecureRandom()
//      val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
//      var t = 0
//      val logl = 7
//      val l = scala.math.pow(2, logl).toInt
//      //println(l.toString + " time steps")
//      t = 0
//      val seed1 = rnd.generateSeed(32)
//      val seed2 = rnd.generateSeed(32)
//      val sk = kes.sumCompositionGenerateKey(seed1, logl)
//      val pk = kes.sumCompositionGetPublicKey(sk)
//      val sk2 = kes.sumCompositionGenerateKey(seed2, logl)
//      val pk2 = kes.sumCompositionGetPublicKey(sk2)
//      //println("tree: " + sk)
//      //println("Target Private Key Length:")
//      //println(kes.skBytes * logl + 2 * kes.pkBytes + 3 * kes.hashBytes * logl)
//      //println("Tree Byte Length:")
//      var data: Array[Byte] = Array()
//      for (item <- sk.toSeq)
//        data = data ++ item
//      //println(data.length)
//      //println("Private Key Time Step:")
//      //println(kes.getSumCompositionKeyTimeStep(sk))
//      //println("Verifying key pair")
//      require(kes.sumCompositionVerifyKeyPair(sk, pk), "Keypair did not validate")
//      //println("Private Key Update:")
//      t += (l * 3) / 4 + 1
//      val sk_evolved = kes.sumCompositionUpdate(sk, t)
//      //println("Key t: " + kes.getSumCompositionKeyTimeStep(sk_evolved).toString)
//      //println("t: " + t.toString)
//      //println("Tree height: " + sk.height.toString)
//      //println("Verifying key pair")
//      require(
//        pk sameElements kes.sumCompositionGetPublicKey(sk_evolved),
//        "Evolved Key did not produce the same public key as original"
//      )
//      require(
//        kes.sumCompositionVerifyKeyPair(sk_evolved, pk),
//        "Evolved Key did not produce the same public key as original"
//      )
//      require(!kes.sumCompositionVerifyKeyPair(sk2, pk), "Invalid keypair verified sumVerifyKeyPair")
//      require(!kes.sumCompositionVerifyKeyPair(sk, pk2), "Invalid keypair verified sumVerifyKeyPair")
//      require(kes.sumCompositionVerifyKeyPair(sk2, pk2), "Valid Key pair failed to validate sumVerifyKeyPair")
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    passed shouldBe true
//  }
//
//  property("Sum Composition Signature Test") {
//    val passed = Try {
//      val rnd: SecureRandom = new SecureRandom()
//      val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
//      var t = 0
//      val message = rnd.generateSeed(2048)
//      val logl = 7
//      val l = scala.math.pow(2, logl).toInt
//      //println(l.toString + " time steps")
//      val seed1 = rnd.generateSeed(32)
//      val seed2 = rnd.generateSeed(32)
//      var sk1 = kes.sumCompositionGenerateKey(seed1, logl)
//      val pk1 = kes.sumCompositionGetPublicKey(sk1)
//      var sk2 = kes.sumCompositionGenerateKey(seed2, logl)
//      val pk2 = kes.sumCompositionGetPublicKey(sk2)
//      t = (l * 1) / 4 + 1
//      sk1 = kes.sumCompositionUpdate(sk1, t)
//      sk2 = kes.sumCompositionUpdate(sk2, t)
//      val sig11 = kes.sumCompositionSign(sk1, message, t)
//      val sig21 = kes.sumCompositionSign(sk2, message, t)
//      require(kes.sumCompositionVerify(pk1, message, sig11, t), "Signature 11 did not verify")
//      require(kes.sumCompositionVerify(pk2, message, sig21, t), "Signature 21 did not verify")
//      t = (l * 3) / 4 + 1
//      sk1 = kes.sumCompositionUpdate(sk1, t)
//      sk2 = kes.sumCompositionUpdate(sk2, t)
//      val sig12 = kes.sumCompositionSign(sk1, message, t)
//      val sig22 = kes.sumCompositionSign(sk2, message, t)
//      require(kes.sumCompositionVerify(pk1, message, sig12, t), "Signature 12 did not verify")
//      require(kes.sumCompositionVerify(pk2, message, sig22, t), "Signature 22 did not verify")
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    passed shouldBe true
//  }
//
//  property("Asymmetric Product Composition Test") {
//    val passed = Try {
//      //println("Testing MMM asymmetric product keys")
//      val rnd: SecureRandom = new SecureRandom()
//      var t = 0L
//      val message = rnd.generateSeed(2048)
//      val seed1 = rnd.generateSeed(32)
//      var prodKey = keys.AsymmetricKey.newFromSeed(seed1, 0)
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("Updating MMM product key")
//      t += 1
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("t: " + t.toString)
//      t += 10
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("t: " + t.toString)
//      //println("product sign")
//      var sigProd = prodKey.sign(message)
//      //println("product verify")
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//      t += 100
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//
//      sigProd = prodKey.sign(message)
//      //println("product verify")
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//      t += 2000
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//
//      t += 10400
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      t += 100000
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//
//      sigProd = prodKey.sign(message)
//      //println("product verify")
//      assert(OpCertVerifier.verify(message, sigProd, t))
//      //println("Product key time step: " + prodKey.timeStep.toString)
//      //println("t: " + t.toString)
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    passed shouldBe true
//  }
//
//  property("Symmetric Product Composition Test") {
//    val passed = Try {
//      //Console.println("Testing MMM asymmetric product keys")
//      val rnd: SecureRandom = new SecureRandom()
//      //dummy sign operation
//      def sign(m: Array[Byte]): Signature =
//        Signature(m)
//      var t = 0
//      val message = rnd.generateSeed(2048)
//      val seed1 = rnd.generateSeed(32)
//      var prodKey = keys.SymmetricKey.newFromSeed(seed1, 0, bytes => Proofs.SignatureEd25519(Sized.strictUnsafe(bytes)))
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("Updating MMM product key")
//      t += 1
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("t: " + t.toString)
//      t += 10
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStepPlusOffset)
//      //println("t: " + t.toString)
//      //println("product sign")
//      var sigProd = prodKey.sign(message)
//      //println("product verify")
//      //println(prodKey.timeStep)
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//      t += 100
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//
//      sigProd = prodKey.sign(message)
//      //println("product verify")
//      assert(OpCertVerifier.verify(message, sigProd, t))
//
//      t += 2000
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//
//      t += 10400
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      t += 100000
//      //println("Updating MMM product key")
//      prodKey = prodKey.update(t)
//      //println("Product key time step:")
//      //println(prodKey.timeStep)
//
//      sigProd = prodKey.sign(message)
//      //println("product verify")
//      assert(OpCertVerifier.verify(message, sigProd, t))
//      //println("Product key time step: " + prodKey.timeStep.toString)
//      //println("t: " + t.toString)
//    } match {
//      case Success(_) =>
//        true
//      case Failure(e) =>
//        e.printStackTrace()
//        false
//    }
//    passed shouldBe true
//  }
//}
