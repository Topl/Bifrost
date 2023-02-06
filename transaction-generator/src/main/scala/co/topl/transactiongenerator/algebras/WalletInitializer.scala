package co.topl.transactiongenerator.algebras

import co.topl.transactiongenerator.models.Wallet

trait WalletInitializer[F[_]] {

  /**
   * Create a Wallet that contains spendable UTxOs
   */
  def initialize: F[Wallet]
}
