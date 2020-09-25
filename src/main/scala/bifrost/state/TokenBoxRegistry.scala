package bifrost.state

import java.io.File

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }

import scala.util.Try

case class TokenBoxRegistry ( private val storage: LSMStore )
  extends Registry[TokenBoxRegistry.K, TokenBoxRegistry.V] {

  import TokenBoxRegistry.{ K, V }

  //----- input and output transformation functions
  override def registryInput ( key: K ): Array[Byte] = key.pubKeyBytes

  override def registryOutput ( value: Array[Byte] ): V = value

  // overload the lookup method by specifying the number of bytes to read
  def lookup (key: K): Seq[V] = lookup(key, BoxId.size)

  /**
   * @param newVersion - block id
   * @param updates the set of updates to be placed in the Token Box Registry
   * @param nodeKeys set of node keys that denote the state this node will maintain (useful for personal wallet nodes)
   * @return - instance of updated TokenBoxRegistry
   *
   *         Runtime complexity of below function is O(MN) + O(L)
   *         where M = Number of boxes to remove
   *         N = Number of boxes owned by a public key
   *         L = Number of boxes to append
   */
  //noinspection ScalaStyle
  override def updateFromState ( newVersion: VersionTag,
                                 updates: TokenRegistryChanges,
                                 nodeKeys: Option[Set[PublicKey25519Proposition]]
                               ): Try[TokenBoxRegistry] = {

    Try {
      log.debug(s"${Console.GREEN} Update TokenBoxRegistry to version: ${newVersion.toString}${Console.RESET}")

      def filterByNodeKeys(updates: Map[K, Seq[V]]): Map[K, Seq[V]] = nodeKeys match {
        case Some(keys) => updates.filter(b => keys.contains(b._1))
        case None       => updates
      }

      val (filteredRemove, filteredAppend) = (filterByNodeKeys(updates.toRemove), filterByNodeKeys(updates.toAppend))

      // look for addresses that will be empty after the update
      val (deleted: Seq[K], updated: Seq[(K, Set[V])]) = {
        // make a list of all accounts to consider then loop through them and determine their new state
        (filteredRemove.keys ++ filteredAppend.keys).map(key => {
          val current = lookup(key)

          // case where the account no longer has any boxes
          if (current.length == filteredRemove(key).length && filteredAppend(key).isEmpty) {
            (Some(key), None)
          }

          // case where the account was initially empty and now has boxes
          else if (current.isEmpty) {
            (None, Some((key, filteredAppend(key).toSet)))

          // case for updating the set of boxes for an account
          } else {
            (None, Some((key, (current ++ filteredAppend(key)).filterNot(filteredRemove(key).toSet).toSet)))
          }
        })
      }.foldLeft((Seq[K](), Seq[(K, Set[V])]()))((acc, acct) => (acc._1 ++ acct._1, acc._2 ++ acct._2))

      storage.update(
        ByteArrayWrapper(newVersion.hashBytes),
        deleted.map(k => ByteArrayWrapper(registryInput(k))),
        updated.map(elem => ByteArrayWrapper(registryInput(elem._1)) -> ByteArrayWrapper(elem._2.flatten.toArray))
        )

      TokenBoxRegistry(storage)
    }
  }

  override def rollbackTo ( version: VersionTag ): Try[TokenBoxRegistry] = Try {
    if ( storage.lastVersionID.exists(_.data sameElements version.hashBytes) ) {
      this
    } else {
      log.debug(s"Rolling back TokenBoxRegistry to: ${version.toString}")
      storage.rollback(ByteArrayWrapper(version.hashBytes))
      TokenBoxRegistry(storage)
    }
  }

}

object TokenBoxRegistry extends Logging {

  type K = PublicKey25519Proposition
  type V = Array[Byte]

  def readOrGenerate ( settings: AppSettings, stateStore: LSMStore ): Option[TokenBoxRegistry] = {
    val tbrDir = settings.tbrDir
    val logDir = settings.logDir
    tbrDir.map(readOrGenerate(_, logDir, settings, stateStore))
  }

  def readOrGenerate ( tbrDir   : String,
                       logDirOpt: Option[String],
                       settings : AppSettings
                     ): TokenBoxRegistry = {

    val iFile = new File(s"$tbrDir")
    iFile.mkdirs()
    val tbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run ( ): Unit = {
        log.info("Closing tokenBoxRegistry storage...")
        tbrStore.close()
      }
    })

    TokenBoxRegistry(tbrStore)
  }

}
