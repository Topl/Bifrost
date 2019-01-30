package bifrost.crypto

import bifrost.crypto.hash.FastCryptographicHash
import bifrost.keygen.KeyFile
import scala.io.Source
import bifrost.keygen.KeyFile.uuid
import scorex.crypto.hash.Sha256
import scorex.crypto.encode.Base58
import scala.reflect.io.Path
import scala.util.Try
import org.scalatest._
import scala.math.BigInt

/*
Creates a key file with a given seed for testing
with the Bitcoin Improvement Project 39 specification
 */

class Bip39Spec extends FlatSpec with Matchers{

  "A wordlist" should "be populated" in {
    val lang = "english.txt"
    val wordListDir = "src/main/resources/bip-0039/"
    val wordList = Source.fromFile(wordListDir + lang).getLines.toList
  }

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
    val sk = myKey.getPrivateKey(password)
  }

  "A seed phrase" should "be generated" in {
    val lang = "english.txt"
    val wordListDir = "src/main/resources/bip-0039/"
    val wordList = Source.fromFile(wordListDir + lang).getLines.toList
    val uuidString = uuid
    val toRemove = "-".toSet
    val seed = uuidString.filterNot(toRemove)
    val seedBytes: Array[Byte] = seed.grouped(2).toArray map { Integer.parseInt(_, 16).toByte }
    val seedHash: Array[Byte] = Sha256.hash(seedBytes)
    val seedBin: Array[String] = seedBytes.map(BigInt(_).toString(2))
    val seedHashBin: Array[String] = seedHash.map(BigInt(_).toString(2))

    def toBinary(b: Byte): String = String.format("%8s", BigInt(b).toString(2) ).replace(' ', '0')

  }
}
