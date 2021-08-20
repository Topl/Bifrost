package co.topl.crypto.signatures

import eddsa.ECVRF25519
import co.topl.crypto.hash.FastCryptographicHash
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import scala.util.{Try,Success,Failure}
import java.security.SecureRandom

class Ed25519VRFSpec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property ("Test all VRF vectors") {
    val passed = Try {
      val vrf:ECVRF25519 = new ECVRF25519
      val fch:FastCryptographicHash = new FastCryptographicHash
      var proof: Array[Byte] = Array()
      var alpha: Array[Byte] = Array()
      var beta: Array[Byte] = Array()
      for (i <- 0 until 5) {
        val sk:Array[Byte] = Array.fill(vrf.SECRET_KEY_SIZE){0x00}
        val pk:Array[Byte] = Array.fill(vrf.PUBLIC_KEY_SIZE){0x00}
        vrf.generatePrivateKey(random = new SecureRandom,sk)
        vrf.generatePublicKey(sk,0,pk,0)
        //println("Private Key")
        //println(binaryArrayToHex(sk))
        //println("Public Key")
        //println(binaryArrayToHex(pk))
        assert(vrf.verifyKeyPair(sk, pk))
        alpha = fch.hash(uuid)
        proof = vrf.vrfProof(sk, alpha)
        beta = vrf.vrfProofToHash(proof)
        //println("Alpha:")
        //println(binaryArrayToHex(alpha))
        assert(vrf.vrfVerify(pk, alpha, proof))
        //println("Proof:")
        //println(binaryArrayToHex(proof))
        //println("Beta:")
        //println(binaryArrayToHex(beta))
      }

      def uuid: String = java.util.UUID.randomUUID.toString

      def binaryArrayToHex(b: Array[Byte]): String = {
        b.map("%02x" format _).mkString
      }

      def hex2bytes(hex: String): Array[Byte] = {
        if (hex.contains(" ")) {
          hex.split(" ").map(Integer.parseInt(_, 16).toByte)
        } else if (hex.contains("-")) {
          hex.split("-").map(Integer.parseInt(_, 16).toByte)
        } else {
          hex.sliding(2, 2).toArray.map(Integer.parseInt(_, 16).toByte)
        }
      }

      /*
      SK = 9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60
      PK = d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a
      alpha = (the empty string)
      x = 307c83864f2833cb427a2ef1c00a013cfdff2768d980c0a3a520f006904de94f
      try_and_increment succeded on ctr = 0
      H = 5b2c80db3ce2d79cc85b1bfb269f02f915c5f0e222036dc82123f640205d0d24
      k = 647ac2b3ca3f6a77e4c4f4f79c6c4c8ce1f421a9baaa294b0adf0244915130f70
      67640acb6fd9e7e84f8bc30d4e03a95e410b82f96a5ada97080e0f187758d38
      U = k*B =
      a21c342b8704853ad10928e3db3e58ede289c798e3cdfd485fbbb8c1b620604f
      V = k*H =
      426fe41752f0b27439eb3d0c342cb645174a720cae2d4e9bb37de034eefe27ad
      pi = 9275df67a68c8745c0ff97b48201ee6db447f7c93b23ae24cdc2400f52fdb08a1a6ac7ec71bf9c9c76e96ee4675ebff60625af28718501047bfd87b810c2d2139b73c23bd69de66360953a642c2a330a
      beta = a64c292ec45f6b252828aff9a02a0fe88d2fcc7f5fc61bb328f03f4c6c0657a9d26efb23b87647ff54f71cd51a6fa4c4e31661d8f72b41ff00ac4d2eec2ea7b3
       */

      for (i <- 0 until 1) {
        val sk = hex2bytes("9d61b19deffd5a60ba844af492ec2cc44449c5697b326919703bac031cae7f60")
        val pk = hex2bytes("d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a")
        //println("Private Key")
        //println(binaryArrayToHex(sk))
        //println("Public Key")
        //println(binaryArrayToHex(pk))
        assert(vrf.verifyKeyPair(sk, pk))
        alpha = Array()
        proof = vrf.vrfProof(sk, alpha)
        val proofTest = hex2bytes("9275df67a68c8745c0ff97b48201ee6db447f7c93b23ae24cdc2400f52fdb08a1a6ac7ec71bf9c9c76e96ee4675ebff60625af28718501047bfd87b810c2d2139b73c23bd69de66360953a642c2a330a")
        assert(proofTest sameElements  proof)
        beta = vrf.vrfProofToHash(proof)
        val betaTest = hex2bytes("a64c292ec45f6b252828aff9a02a0fe88d2fcc7f5fc61bb328f03f4c6c0657a9d26efb23b87647ff54f71cd51a6fa4c4e31661d8f72b41ff00ac4d2eec2ea7b3")
        assert(betaTest sameElements beta)
        //println("Alpha:")
        //println(binaryArrayToHex(alpha))
        assert(vrf.vrfVerify(pk, alpha, proof))
        //println("Proof:")
        //println(binaryArrayToHex(proof))
        //println("Beta:")
        //println(binaryArrayToHex(beta))
      }

      /*
      SK = 4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb
      PK = 3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c
      alpha = 72 (1 byte)
      x = 68bd9ed75882d52815a97585caf4790a7f6c6b3b7f821c5e259a24b02e502e51
      try_and_increment succeded on ctr = 4
      H = 08e18a34f3923db32e80834fb8ced4e878037cd0459c63ddd66e5004258cf76c
      k = 627237308294a8b344a09ad893997c630153ee514cd292eddd577a9068e2a6f24
      cbee0038beb0b1ee5df8be08215e9fc74608e6f9358b0e8d6383b1742a70628
      U = k*B =
      18b5e500cb34690ced061a0d6995e2722623c105221eb91b08d90bf0491cf979
      V = k*H =
      87e1f47346c86dbbd2c03eafc7271caa1f5307000a36d1f71e26400955f1f627
      pi = 84a63e74eca8fdd64e9972dcda1c6f33d03ce3cd4d333fd6cc789db12b5a7b9d03f1cb6b2bf7cd81a2a20bacf6e1c04e59f2fa16d9119c73a45a97194b504fb9a5c8cf37f6da85e03368d6882e511008
      beta = cddaa399bb9c56d3be15792e43a6742fb72b1d248a7f24fd5cc585b232c26c934711393b4d97284b2bcca588775b72dc0b0f4b5a195bc41f8d2b80b6981c784e
       */

      for (i <- 0 until 1) {
        val sk = hex2bytes("4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb")
        val pk = hex2bytes("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c")
        //println("Private Key")
        //println(binaryArrayToHex(sk))
        //println("Public Key")
        //println(binaryArrayToHex(pk))
        assert(vrf.verifyKeyPair(sk, pk))
        alpha = Array(0x72.toByte)
        proof = vrf.vrfProof(sk, alpha)
        val proofTest = hex2bytes("84a63e74eca8fdd64e9972dcda1c6f33d03ce3cd4d333fd6cc789db12b5a7b9d03f1cb6b2bf7cd81a2a20bacf6e1c04e59f2fa16d9119c73a45a97194b504fb9a5c8cf37f6da85e03368d6882e511008")
        assert(proofTest sameElements proof)
        beta = vrf.vrfProofToHash(proof)
        val betaTest = hex2bytes("cddaa399bb9c56d3be15792e43a6742fb72b1d248a7f24fd5cc585b232c26c934711393b4d97284b2bcca588775b72dc0b0f4b5a195bc41f8d2b80b6981c784e")
        assert(betaTest sameElements beta)
        //println("Alpha:")
        //println(binaryArrayToHex(alpha))
        assert(vrf.vrfVerify(pk, alpha, proof))
        //println("Proof:")
        //println(binaryArrayToHex(proof))
        //println("Beta:")
        //println(binaryArrayToHex(beta))
      }

      /*
      SK = c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7
      PK = fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025
      alpha = af82 (2 bytes)
      x = 909a8b755ed902849023a55b15c23d11ba4d7f4ec5c2f51b1325a181991ea95c
      try_and_increment succeded on ctr = 0
      H = e4581824b70badf0e57af789dd8cf85513d4b9814566de0e3f738439becfba33
      k = a950f736af2e3ae2dbcb76795f9cbd57c671eee64ab17069f945509cd6c4a7485
      2fe1bbc331e1bd573038ec703ca28601d861ad1e9684ec89d57bc22986acb0e
      U = k*B =
      5114dc4e741b7c4a28844bc585350240a51348a05f337b5fd75046d2c2423f7a
      V = k*H =
      a6d5780c472dea1ace78795208aaa05473e501ed4f53da57e1fb13b7e80d7f59
      pi = aca8ade9b7f03e2b149637629f95654c94fc9053c225ec21e5838f193af2b727b84ad849b0039ad38b41513fe5a66cdd2367737a84b488d62486bd2fb110b4801a46bfca770af98e059158ac563b690f
      beta = d938b2012f2551b0e13a49568612effcbdca2aed5d1d3a13f47e180e01218916e049837bd246f66d5058e56d3413dbbbad964f5e9f160a81c9a1355dcd99b453
       */

      for (i <- 0 until 1) {
        val sk = hex2bytes("c5aa8df43f9f837bedb7442f31dcb7b166d38535076f094b85ce3a2e0b4458f7")
        val pk = hex2bytes("fc51cd8e6218a1a38da47ed00230f0580816ed13ba3303ac5deb911548908025")
        //println("Private Key")
        //println(binaryArrayToHex(sk))
        //println("Public Key")
        //println(binaryArrayToHex(pk))
        assert(vrf.verifyKeyPair(sk, pk))
        alpha = Array(0xaf.toByte, 0x82.toByte)
        proof = vrf.vrfProof(sk, alpha)
        val proofTest = hex2bytes("aca8ade9b7f03e2b149637629f95654c94fc9053c225ec21e5838f193af2b727b84ad849b0039ad38b41513fe5a66cdd2367737a84b488d62486bd2fb110b4801a46bfca770af98e059158ac563b690f")
        assert(proofTest sameElements proof)
        beta = vrf.vrfProofToHash(proof)
        val betaTest = hex2bytes("d938b2012f2551b0e13a49568612effcbdca2aed5d1d3a13f47e180e01218916e049837bd246f66d5058e56d3413dbbbad964f5e9f160a81c9a1355dcd99b453")
        assert(betaTest sameElements beta)
        //println("Alpha:")
        //println(binaryArrayToHex(alpha))
        assert(vrf.vrfVerify(pk, alpha, proof))
        //println("Proof:")
        //println(binaryArrayToHex(proof))
        //println("Beta:")
        //println(binaryArrayToHex(beta))
      }
    } match {
      case Success(_) => true
      case Failure(e) => e.printStackTrace(); false
    }
    passed shouldBe true
  }

}
