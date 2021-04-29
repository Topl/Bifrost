package http

import pureconfig._
import pureconfig.generic.auto._
import pureconfig.generic.semiauto.deriveReader

/** A case class used in AppSettings to note the state of the different API endpoints */
case class NamespaceSelector(
  private val wallet:       Boolean,
  private val onlineWallet: Boolean
) {

  lazy val namespaceStates: Map[Namespace, Boolean] =
    Map(
      WalletNamespace       -> wallet,
      OnlineWalletNamespace -> onlineWallet
    )
}

object NamespaceSelector {

  implicit val namespaceSelectorReader: ConfigReader[NamespaceSelector] =
    ConfigReader.forProduct2("wallet", "onlineWallet")(NamespaceSelector(_, _))
}

/**
 * A generic class to be shared by all namespaces that provides a common name value used for pattern matching
 * in the end-point handler partial function
 *
 * @param name - the name to be used for particular end-point handler
 */
sealed abstract class Namespace(val name: String)

/**
 * The namespace for "offline" wallet requests.
 * Currently includes [[GjallarhornOfflineApiRoute]] and [[KeyManagementApiRoute]]
 */
case object WalletNamespace extends Namespace("wallet")

/** The namespace for "online" wallet requests. Currently includes [[GjallarhornOnlineApiRoute]] */
case object OnlineWalletNamespace extends Namespace("onlineWallet")
