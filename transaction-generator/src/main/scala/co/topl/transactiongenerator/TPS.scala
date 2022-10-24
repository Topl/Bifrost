package co.topl.transactiongenerator

/**
 * Definitions for other blockchains' transaction-per-second
 *
 * TPS is not a particularly useful metric.  Each transaction is unique, like a snowflake.  Two different transactions
 * may have different sizes, different impact, or different computational complexity.  As such, one transaction may
 * take longer to propagate, process, and settle into a chain than another transaction.  A TPS metric assumes all
 * Transactions are effectively the same, which would only accomplish the world's most boring blockchain.
 *
 * But for the sake of argument, here are some TPS values.
 */
object TPS {

  // Reference: https://crypto.com/university/blockchain-scalability
  final val Visa = 24000
  final val Ripple = 1500
  final val PayPal = 193
  final val BitcoinCash = 60
  final val Litecoin = 56
  final val Dash = 48
  final val Ethereum = 20
  final val Bitcoin = 7

  // Reference: https://www.ledger.com/academy/what-is-cardano
  final val Cardano = 250

}
