package co.topl.it.util

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.{Address, PrivateKeyCurve25519}
import co.topl.consensus.KeyRing
import co.topl.crypto.KeyfileCurve25519
import co.topl.settings.NetworkType.PrivateNet

import scala.util.{Failure, Success, Try}

object KeyCreator {

  implicit val networkPrefix: NetworkPrefix = PrivateNet.netPrefix
  val keyFileDir = ".bifrost/private-test-keys"
  val keyRing = KeyRing[PrivateKeyCurve25519, KeyfileCurve25519](keyFileDir, KeyfileCurve25519)

  def generateKeys(): Try[Set[Address]] =
  // generate a new random key pair and save to disk
    keyRing.generateNewKeyPairs(50, Some("test")).map { sk =>
      sk.map { s =>
        keyRing.exportKeyfile(s.publicImage.address, "test")
        s.publicImage.address
      }
    }

  def main(args: Array[String]): Unit = {
    generateKeys() match {
      case Success(addrs) => println(addrs)
      case Failure(e)     => throw e
    }
  }

}

