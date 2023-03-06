package co.topl.crypto.signing

trait KeyEvolvingSignatureScheme[SK, VK, SIG, H] {

  def createKeyPair(seed: Array[Byte], height: H, offset: Long): (SK, VK)

  def sign(privateKey: SK, message: Array[Byte]): SIG

  def verify(signature: SIG, message: Array[Byte], verifyKey: VK): Boolean

  def update(privateKey: SK, steps: Int): SK

  def getCurrentStep(privateKay: SK): Int

  def getMaxStep(privateKey: SK): Int

  def getVerificationKey(privateKey: SK): VK
}
