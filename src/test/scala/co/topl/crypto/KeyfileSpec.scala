package co.topl.crypto

import co.topl.attestation.Address
import co.topl.utils.ValidGenerators
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.util.{Failure, Success}

class KeyfileSpec
  extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with ValidGenerators
    with Matchers {

//  keyRing.generateKeyFile(password)
  //  keyRing.generateNewKeyPairs(num, seed)
//  keyRing.lockKeyFile(addr, password)
//  keyRing.unlockKeyFile(addr, password)
//  keyRing.exportKeyfile(addr, password)
//  keyRing.importPhrase(password, mnemonic, lang)
//  keyRing.addresses
//  keyRing.lookupPublicKey(matchingAddr)
//  keyRing.signWithAddress(matchingAddr, messageToSign)

  val password: String = stringGen.sample.get
  val messageByte: Array[Byte] = nonEmptyBytesGen.sample.get


  //TODO: Jing - Maybe generate the testing keyfiles in a separate directory
  val address: Address = keyRing.generateKeyFile(password) match {
    case Success(addr) => addr
    case Failure(ex) => throw new Error(s"An error occurred while creating a new keyfile. $ex")
  }

  property("The randomly generated address from generateKeyFile should exist in keyRing") {
    keyRing.addresses.contains(address) shouldBe true
  }

  property("Once we lock the generated address, it will be removed from the secrets set in the keyRing") {
    keyRing.lockKeyFile(address.toString, password)
    keyRing.lockKeyFile(address.toString, password)

    keyRing.addresses.contains(address) shouldBe false
  }

  property("Once we unlock, the address will be accessible from the keyRing again") {
    keyRing.unlockKeyFile(address.toString, password)
    keyRing.unlockKeyFile(address.toString, password)

    keyRing.addresses.contains(address) shouldBe true
  }

  property("LookupPublickKey should return the correct public key to the address") {
    keyRing.lookupPublicKey(address).get.address shouldEqual address
  }

  property("") {
//    keyRing.importPhrase(password, mnemonic, lang)
//    signWithAddress is used in ValidGenerators for validAssetTransfer, and validated by calling syntacticValidate
//    val signature = keyRing.signWithAddress(address, messageByte)
  }
}
