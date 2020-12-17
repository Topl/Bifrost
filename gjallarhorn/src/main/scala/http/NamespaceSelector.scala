package http

/** A case class used in AppSettings to note the state of the differnet API endpoints*/
case class NamespaceSelector(
  private val wallet:  Boolean,
  private val onlineWallet: Boolean
) {

  lazy val namespaceStates: Map[Namespace, Boolean] =
    Map(
      WalletNamespace -> wallet,
      OnlineWalletNamespace -> onlineWallet
    )
}

/** a generic class to be shared by all namespaces that provides a common name value used for pattern matching
  * in the end-point handler partial function */
sealed abstract class Namespace(val name: String)

case object WalletNamespace extends Namespace("wallet")
case object OnlineWalletNamespace extends Namespace("onlineWallet")

