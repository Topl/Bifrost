package co.topl.it.util

import co.topl.attestation.Address
import co.topl.keyManagement.{KeyRing, KeyfileCurve25519, PrivateKeyCurve25519}
import co.topl.utils.Logging
import co.topl.utils.NetworkType._

import scala.util.Try

object KeyCreator extends Logging {

  def randomString(length: Int): String = scala.util.Random.alphanumeric.take(length).mkString

  def generateKeys(num: Int, seed: Option[String], pass: => String, dir: String)(implicit
    networkPrefix:      NetworkPrefix
  ): Try[Set[(Address, String)]] =
    // generate a new random key pair and save to disk
    KeyRing
      .empty[PrivateKeyCurve25519, KeyfileCurve25519](Some(dir))(
        networkPrefix,
        PrivateKeyCurve25519.secretGenerator,
        KeyfileCurve25519
      )
      .generateNewKeyPairs(num, seed)
      .map {
        _.map { sk =>
          val ps = pass
          KeyfileCurve25519.saveToDisk(dir, ps, sk)
          sk.publicImage.address -> ps
        }
      }

  def printRes(label: String, data: Set[(Address, String)]): Unit = {
    log.info(s"\n$label")
    data.foreach { case (addr, pass) => log.info(s"${addr.toString}\t\t$pass") }
  }

  def main(args: Array[String]): Unit = {
    val basePath = "~/"

    lazy val privateKeys =
      generateKeys(50, Some("test"), "test", basePath + "private-testnet")(PrivateTestnet.netPrefix)
    lazy val localKeys = generateKeys(50, Some("test"), "test", basePath + "local-testnet")(LocalTestnet.netPrefix)
    lazy val helKeys = generateKeys(50, None, randomString(12), basePath + "hel-testnet")(HelTestnet.netPrefix)
    lazy val valhallaKeys =
      generateKeys(50, None, randomString(12), basePath + "valhalla-testnet")(ValhallaTestnet.netPrefix)

    log.info("WORKING... (this takes awhile)")
    //    privateKeys.map(printRes("PRIVATE", _))
    //    localKeys.map(printRes("LOCAL", _))
    //    helKeys.map(printRes("HEL", _))
    //    valhallaKeys.map(printRes("VALHALLA", _))
  }

}
