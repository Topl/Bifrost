package co.topl.transactiongenerator.interpreters

import co.topl.transactiongenerator.models.Wallet

object WalletSplitter {

  /**
   * Split a wallet into `subCount` groups of equal size
   */
  def split(wallet: Wallet, subCount: Int): Vector[Wallet] = {
    val actualSubCount = subCount.min(wallet.spendableBoxIds.size)
    if (actualSubCount > 1) {
      val spendable = wallet.spendableBoxIds.toVector
      spendable.grouped(spendable.size / actualSubCount).map(g => wallet.copy(g.toMap)).toVector
    } else {
      Vector(wallet)
    }
  }
}
