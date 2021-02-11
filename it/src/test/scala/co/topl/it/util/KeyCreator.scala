package co.topl.it.util

import co.topl.attestation.Address
import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.crypto.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.settings.NetworkType._

import scala.util.Try

object KeyCreator {

  def randomString(length: Int): String = scala.util.Random.alphanumeric.take(length).mkString

  def generateKeys(num: Int, seed: Option[String], pass: => String, dir: String)(implicit
    networkPrefix:      NetworkPrefix
  ): Try[Set[(Address, String)]] = {
    // generate a new random key pair and save to disk
    KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](dir, KeyfileCurve25519)
      .generateNewKeyPairs(num, seed).map {
      _.map { sk =>
        KeyfileCurve25519.saveToDisk(dir, pass, sk)
        sk.publicImage.address -> pass
      }
    }
  }

  def printRes(label: String, data: Set[(Address, String)]): Unit = {
    println(s"\n${label}")
    data.foreach { case(addr, pass) => println(s"$addr\t\t$pass") }
  }

  def main(args: Array[String]): Unit = {
    lazy val privateKeys = generateKeys(50, Some("test"), "test", "./private-test-keys")(PrivateNet.netPrefix)
    lazy val localKeys = generateKeys(50, Some("test"), "test", "./local-test-keys")(LocalNet.netPrefix)
    lazy val helKeys = generateKeys(50, None, randomString(12), "./hel-keys")(DevNet.netPrefix)
    lazy val valhallaKeys = generateKeys(50, None, randomString(12), "./valhalla-keys")(TestNet.netPrefix)

    println("WORKING... (this takes awhile)")
    privateKeys.map(printRes("PRIVATE", _))
    localKeys.map(printRes("LOCAL", _))
    helKeys.map(printRes("HEL", _))
    valhallaKeys.map(printRes("VALHALLA", _))
  }

}
