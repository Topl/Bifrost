package co.topl.attestation.keyManagement.stakingKeys

import co.topl.attestation.SignatureEd25519
import co.topl.crypto.kes
import co.topl.attestation.keyManagement.derivedKeys.{
  DerivedKeyIndex,
  ExtendedPrivateKeyEd25519,
  ExtendedPublicKeyEd25519,
  SoftIndex
}
import co.topl.attestation.keyManagement.mnemonic
import co.topl.crypto.kes.keys.SymmetricKey
import scala.collection.mutable

class ExtendedKES(vks: ExtendedPublicKeyEd25519, sksSet: mutable.Map[SoftIndex, ExtendedPrivateKeyEd25519]) {

  def deriveVks(index: SoftIndex): ExtendedPublicKeyEd25519 =
    vks.derive(index)

  def getNextKESKey: (SignatureEd25519, SymmetricKey) = {
    val minIndex = SoftIndex(sksSet.keys.map(_.value).min)
    val sksMin = sksSet(minIndex)
    val sk_KES = kes.keys.SymmetricKey.newFromSeed(sksMin.leftKey.value.toArray, minIndex.value)
    val vk_KES = sk_KES.getVerificationKey
    val sign_vk_KES = sksMin.sign(vk_KES.bytes)
    sksSet.remove(minIndex)
    (sign_vk_KES, sk_KES)
  }

}

object ExtendedKES {

  def apply(n: Int): ExtendedKES = {
    val sks = ExtendedPrivateKeyEd25519(mnemonic.Entropy.fromUuid(java.util.UUID.randomUUID()), "")
    val sksIndex = Array.range(0, n).map(DerivedKeyIndex.soft)
    new ExtendedKES(
      sks.public,
      mutable.Map(sksIndex.zip(sksIndex.map(sks.derive(_).toOption.get)).toSeq: _*)
    )
  }

}
