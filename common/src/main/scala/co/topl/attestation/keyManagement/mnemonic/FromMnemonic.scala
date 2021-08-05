package co.topl.attestation.keyManagement.mnemonic

import simulacrum.typeclass

/**
 * A type-class representing a type that can be generated from a mnemonic.
 * @tparam T the type that can be generated from a mnemonic
 */
@typeclass
trait FromMnemonic[T] {

  /**
   * Derives a value `T` from a given mnemonic entropy and phrase.
   * @param entropy the underlying entropy of the mnemonic
   * @param phrase the words of the mnemonic phrase
   * @return a generated value `T`
   */
  def deriveFrom(entropy: Mnemonic.Entropy, phrase: Mnemonic.Phrase): T
}
