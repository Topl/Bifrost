package bifrost.crypto

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.keygen.KeyFile
import bifrost.keygen.KeyFile.uuid
import scorex.crypto.encode.Base58
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._

/*
Creates a key file with a given seed for testing
with the Bitcoin Improvement Project 39 specification
 */

class Bip39Spec extends FlatSpec with Matchers{

  "A key file" should "be generated" in {
    val keyFileDir = "/tmp/scorex/test-data/keyfiles/bip39test"
    val path: Path = Path(keyFileDir)
    Try(path.deleteRecursively())
    Try(path.createDirectory())

    val password = "password"
    val seed = "seed"

    val seedBytes: Array[Byte] = seed.getBytes
    val seedHash: Array[Byte] = FastCryptographicHash(seed)

    val myKey = KeyFile(password, seedHash, keyFileDir)

  }
}
