package bifrost.state

import java.io.File

import bifrost.modifier.box.TokenBox
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }

import scala.util.{ Failure, Success, Try }

/**
 * A registry containing mappings from public keys to a sequence of boxIds
 *
 * @param storage Persistent storage object for saving the TokenBoxRegistry to disk
 * @param nodeKeys set of node keys that denote the state this node will maintain (useful for personal wallet nodes)
 */
class TokenBoxRegistry ( protected val storage: LSMStore,
                         nodeKeys: Option[Set[PublicKey25519Proposition]]
                       ) extends Registry[TokenBoxRegistry.K, TokenBoxRegistry.V] {

  import TokenBoxRegistry.{ K, V }

  //----- input and output transformation functions
  override protected def registryInput ( key: K ): Array[Byte] = key.pubKeyBytes

  override protected def registryOutput ( value: Array[Byte] ): Seq[V] = value.grouped(BoxId.size).toSeq.map(v => BoxId(v))

  override protected def registryOut2StateIn (value: V): Array[Byte] = value.hashBytes

  protected[state] def getBox(key: K, state: SR): Option[Seq[TokenBox]] = super.getBox[TokenBox](key, state)


  /**
   * @param newVersion - block id
   * @param toRemove map of public keys to a sequence of boxIds that should be removed
   * @param toAppend map of public keys to a sequence of boxIds that should be added
   * @return - instance of updated TokenBoxRegistry
   *
   *         Runtime complexity of below function is O(MN) + O(L)
   *         where M = Number of boxes to remove
   *         N = Number of boxes owned by a public key
   *         L = Number of boxes to append
   */
  protected[state] def update ( newVersion: VersionTag,
                                toRemove: Map[K, Seq[V]],
                                toAppend: Map[K, Seq[V]]
                              ): Try[TokenBoxRegistry] = {

    Try {
      def filterByNodeKeys(updates: Map[K, Seq[V]]): Map[K, Seq[V]] = nodeKeys match {
        case Some(keys) => updates.filter(b => keys.contains(b._1))
        case None       => updates
      }

      val (filteredRemove, filteredAppend) = (filterByNodeKeys(toRemove), filterByNodeKeys(toAppend))

      // determine the new state of each account from the changes
      val (deleted: Seq[K], updated: Seq[(K, Set[V])]) = {

        // make a list of all accounts to consider then loop through them and determine their new state
        (filteredRemove.keys ++ filteredAppend.keys).map(key => {
          val current = lookup(key).getOrElse(Seq())

          // case where the account no longer has any boxes
          if ( current.forall(filteredRemove.getOrElse(key, Seq()).contains) && filteredAppend.getOrElse(key, Seq()).isEmpty ) {
            (Some(key), None)

          // case where the account was initially empty and now has boxes
          } else if (current.isEmpty && filteredAppend.getOrElse(key, Seq()).nonEmpty) {
            (None, Some((key, filteredAppend(key).toSet)))

          // case for updating the set of boxes for an account
          } else if (filteredAppend.getOrElse(key, List()).nonEmpty) {
            val idsToRemove = filteredRemove.getOrElse(key, Seq())
            val idsToAppend = filteredAppend.getOrElse(key, Seq())
            val newIds = (current.filterNot(idsToRemove.contains(_)) ++ idsToAppend).toSet
            (None, Some((key, newIds)))

          // case for genesis account where there are no previous boxes and nothing to remove or append
          } else (None, None)
        })
      }.foldLeft((Seq[K](), Seq[(K, Set[V])]()))((acc, acct) => (acc._1 ++ acct._1, acc._2 ++ acct._2))

      storage.update(
        ByteArrayWrapper(newVersion.hashBytes),
        deleted.map(k => ByteArrayWrapper(registryInput(k))),
        updated.map {
          case (key, value) => ByteArrayWrapper(registryInput(key)) -> ByteArrayWrapper(value.toSeq.flatMap(_.hashBytes).toArray)
        })

    } match {
      case Success(_) =>
        log.debug(s"${Console.GREEN} Update TokenBoxRegistry to version: ${newVersion.toString}${Console.RESET}")
        Success(new TokenBoxRegistry(storage, nodeKeys))

      case Failure(ex) => Failure(ex)
    }
  }

  override def rollbackTo ( version: VersionTag ): Try[TokenBoxRegistry] = Try {
    if ( storage.lastVersionID.exists(_.data sameElements version.hashBytes) ) {
      this
    } else {
      log.debug(s"Rolling back TokenBoxRegistry to: ${version.toString}")
      storage.rollback(ByteArrayWrapper(version.hashBytes))
      new TokenBoxRegistry(storage, nodeKeys)
    }
  }
}

object TokenBoxRegistry extends Logging {

  type K = PublicKey25519Proposition
  type V = BoxId

  def readOrGenerate ( settings: AppSettings, nodeKeys: Option[Set[PublicKey25519Proposition]] ): Option[TokenBoxRegistry] = {
    if (settings.enableTBR) {
      log.info("Initializing state with Token Box Registry")

      val dataDir = settings.dataDir.ensuring(_.isDefined, "data dir must be specified").get

      val iFile = new File(s"$dataDir/tokenBoxRegistry")
      iFile.mkdirs()
      val storage = new LSMStore(iFile)

      Some(new TokenBoxRegistry(storage, nodeKeys))

    } else None
  }
}
