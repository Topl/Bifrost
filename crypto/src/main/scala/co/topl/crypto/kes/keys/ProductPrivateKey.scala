package co.topl.crypto.kes.keys

import co.topl.crypto.kes.signatures.ProductSignature

abstract class ProductPrivateKey {
  def update(globalTimeStep:Long): ProductPrivateKey
  def sign(message:Array[Byte]): ProductSignature
  def getVerificationKey:PublicKey
  def timeStepPlusOffset:Long
  def timeStep:Long
}