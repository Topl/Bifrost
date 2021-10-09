//package co.topl.crypto.kes.keys
//
//import co.topl.crypto.kes.KeyEvolvingSignatureScheme
//import co.topl.crypto.kes.signatures.SumSignature
//import co.topl.crypto.PublicKey
//import co.topl.models.utility.BinaryTree
//
//case class SumPrivateKey(L: BinaryTree[Array[Byte]], offset: Long) {
//
//  import SumPrivateKey._
//
//  def update(t: Int): SumPrivateKey =
//    SumPrivateKey(kes.sumUpdateFast(L, t), offset)
//
//  def sign(kes: KeyEvolvingSignatureScheme, m: Array[Byte]): SumSignature =
//    SumSignature(kes.sumCompositionSign(L, m, kes.getSumCompositionKeyTimeStep(L)), offset, getVerificationKey)
//
//  def getVerificationKey: PublicKey =
//    PublicKey(kes.sumCompositionGetPublicKey(L))
//
//  def timeStep(kes: KeyEvolvingSignatureScheme): Int =
//    kes.getSumCompositionKeyTimeStep(L)
//}
//
//object SumPrivateKey {
//
//  val kes: KeyEvolvingSignatureScheme = new KeyEvolvingSignatureScheme
//
//  def newFromSeed(seed: Array[Byte], height: Int, offset: Long): SumPrivateKey =
//    SumPrivateKey(kes.sumCompositionGenerateKey(seed, height), offset)
//}
