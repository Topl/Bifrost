package bifrost.crypto

import bifrost.keygen.KeyFile
import scorex.crypto.encode.Base58


class BIP39test {
  //val pubKey: Array[Byte] = Array(0)
  //val cipherText: Array[Byte] = Array(0)
  //val mac: Array[Byte] = Array(0)
  //val salt: Array[Byte] = Array(0)
  //val iv: Array[Byte] = Array(0)
  val pubKeyString = "some public key"
  val cipherTextString = "some cipher text"
  val macString = "some mac"
  val saltString = "some salt"
  val ivString = "some iv"

  val pubKey = Base58.decode(pubKeyString).get
  val cipherText = Base58.decode(cipherTextString).get
  val mac = Base58.decode(macString).get
  val salt = Base58.decode(saltString).get
  val iv = Base58.decode(ivString).get

  val password = "some password"
  val seed = "some seed"
  val keyFileDir = "keyfiles/bip39test"
  val myKeyfile = KeyFile(pubKey,cipherText,mac,salt,iv)
  // use of KeyFile companion object and KeyFile case class seems ambiguous in BWallet.scala
  // how does apply() method get arguments passed to it for a KeyFile object?
  // why is KeyFile a case class?
}
