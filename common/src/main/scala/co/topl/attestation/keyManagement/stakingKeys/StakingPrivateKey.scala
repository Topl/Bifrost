package co.topl.attestation.keyManagement.stakingKeys

import co.topl.crypto.hash.FastCryptographicHash
import co.topl.crypto.signatures.eddsa.ECVRF25519
import co.topl.crypto.kes.{KeyEvolvingSignatureScheme, SumPrivateKey}
import co.topl.attestation.keyManagement.PrivateKeyEd25519
import java.security.SecureRandom
import scala.util.Try


case class StakingPrivateKey(sk_vrf:Array[Byte], vk_vrf:Array[Byte], sk_kes:SumPrivateKey) {
  import StakingPrivateKey._

  val publicImage: Array[Byte] = fch.hash(vk_vrf ++ sk_kes.getVerificationKey(kes))

  def generateAddress(signer:PrivateKeyEd25519):Array[Byte] =
    signer.publicImage.bytes ++ this.publicImage ++ signer.sign(this.publicImage)

  def sign(message:Array[Byte]): Array[Byte] = sk_kes.sign(kes, message:Array[Byte])

  def prove(message:Array[Byte]): Array[Byte] = vrf.vrfProof(this.sk_vrf,message)

}

object StakingPrivateKey {

  val vrf = new ECVRF25519
  val kes = new KeyEvolvingSignatureScheme
  val fch = new FastCryptographicHash

  val logl = 7

  private def uuid: String = java.util.UUID.randomUUID.toString

  def createVrfKeypair(seed: Array[Byte]): (Array[Byte],Array[Byte]) = {
    //This algorithm uses SHA-1 as the foundation of the PRNG. It computes the SHA-1 hash over a true-random seed value
    // concatenated with a 64-bit counter which is incremented by 1 for each operation.
    // From the 160-bit SHA-1 output, only 64 bits are used.
    val rnd: SecureRandom = SecureRandom.getInstance("SHA1PRNG")
    rnd.setSeed(seed)
    val sk = Array.fill[Byte](vrf.SECRET_KEY_SIZE){0x00}
    vrf.generatePrivateKey(rnd,sk)
    val pk = Array.fill[Byte](vrf.PUBLIC_KEY_SIZE){0x00}
    vrf.generatePublicKey(sk,0,pk,0)
    (sk,pk)
  }

  def apply(seed:Array[Byte], ledger_id:Array[Byte]):Try[StakingPrivateKey] = Try {
    val newVrfKeys = createVrfKeypair(seed)
    val newKesKey = SumPrivateKey.newFromSeed(kes,fch.hash(uuid),logl)
    StakingPrivateKey(newVrfKeys._1,newVrfKeys._2,newKesKey)
  }
}
