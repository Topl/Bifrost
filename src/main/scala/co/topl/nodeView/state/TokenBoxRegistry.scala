package co.topl.nodeView.state

import co.topl.attestation.Address
import co.topl.modifier.box.{Box, BoxId, TokenBox, TokenValueHolder}
import co.topl.nodeView.state.MinimalState.VersionTag
import co.topl.settings.AppSettings
import co.topl.utils.Logging
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import java.io.File
import scala.util.{Failure, Success, Try}

/**
 * A registry containing mappings from public keys to a sequence of boxIds
 *
 * @param storage Persistent storage object for saving the TokenBoxRegistry to disk
 * @param nodeKeys set of node keys that denote the state this node will maintain (useful for personal wallet nodes)
 */
class TokenBoxRegistry (protected val storage: LSMStore, nodeKeys: Option[Set[Address]])
  extends Registry[TokenBoxRegistry.K, TokenBoxRegistry.V] {

  import TokenBoxRegistry.{K, V}

  //----- input and output transformation functions
  override protected val registryInput: K => Array[Byte] = (key: K) => key.bytes

  override protected val registryOutput: Array[Byte] => Seq[V] =
    (value: Array[Byte]) => value.grouped(Longs.BYTES).toSeq.map(Longs.fromByteArray)

  override protected val registryOut2StateIn: (K, V) => BoxId = (key: K, value: V) => BoxId.idFromEviNonce(key.evidence, value)

  protected[state] def getBox(key: K, state: SR): Option[Seq[TokenBox[TokenValueHolder]]] =
    super.getBox[TokenBox[TokenValueHolder]](key, state)

  /** Helper function to filter updates by node keys if they are present */
  private def filterByNodeKeys(updates: Map[K, Seq[V]]): Map[K, Seq[V]] = nodeKeys match {
    case Some(keys) => updates.filter(b => keys.contains(b._1))
    case None       => updates
  }

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
                              ): Try[TokenBoxRegistry] = Try {

    val (filteredRemove, filteredAppend) = (filterByNodeKeys(toRemove), filterByNodeKeys(toAppend))

    // determine the new state of each account from the changes
    val (deleted: Seq[K], updated: Seq[(K, Set[V])]) = {

      // make a list of all accounts to consider then loop through them and determine their new state
      (filteredRemove.keys ++ filteredAppend.keys).map(key => {
        val current = lookupRaw(key).getOrElse(Seq())

        // case where the account no longer has any boxes
        if ( current.forall(filteredRemove.getOrElse(key, Seq()).contains) && filteredAppend.getOrElse(key, Seq()).isEmpty ) {
          (Some(key), None)

          // case where the account was initially empty and now has boxes
        } else if ( current.isEmpty && filteredAppend.getOrElse(key, Seq()).nonEmpty ) {
          (None, Some((key, filteredAppend(key).toSet)))

          // case for updating the set of boxes for an account
        } else if ( filteredAppend.getOrElse(key, List()).nonEmpty ) {
          val idsToRemove = filteredRemove.getOrElse(key, Seq())
          val idsToAppend = filteredAppend.getOrElse(key, Seq())
          val newIds = (current.filterNot(idsToRemove.contains(_)) ++ idsToAppend).toSet
          (None, Some((key, newIds)))

          // fall through case where there are no previous boxes in state and we're trying to remove boxes that don't exist
        } else throw new Error("Attempted invalid TBR update")
      })
    }.foldLeft((Seq[K](), Seq[(K, Set[V])]()))((acc, acct) => (acc._1 ++ acct._1, acc._2 ++ acct._2))

    storage.update(
      ByteArrayWrapper(newVersion.bytes),
      deleted.map(k => ByteArrayWrapper(registryInput(k))),
      updated.map {
        case (key, value) => ByteArrayWrapper(registryInput(key)) -> ByteArrayWrapper(value.toSeq.flatMap(Longs.toByteArray).toArray)
      })

  } match {
    case Success(_) =>
      log.debug(s"${Console.GREEN} Update TokenBoxRegistry to version: ${newVersion.toString}${Console.RESET}")
      Success(new TokenBoxRegistry(storage, nodeKeys))

    case Failure(ex) => Failure(ex)
  }

  override def rollbackTo (version: VersionTag): Try[TokenBoxRegistry] = Try {
    if ( storage.lastVersionID.exists(_.data sameElements version.bytes) ) {
      this
    } else {
      log.debug(s"Rolling back TokenBoxRegistry to: ${version.toString}")
      storage.rollback(ByteArrayWrapper(version.bytes))
      new TokenBoxRegistry(storage, nodeKeys)
    }
  }
}

object TokenBoxRegistry extends Logging {

  type K = Address
  type V = Box.Nonce

  def readOrGenerate (settings: AppSettings, nodeKeys: Option[Set[Address]]): Option[TokenBoxRegistry] = {
    if (settings.application.enableTBR) {
      log.info("Initializing state with Token Box Registry")

      val dataDir = settings.application.dataDir.ensuring(_.isDefined, "data dir must be specified").get

      val file = new File(s"$dataDir/tokenBoxRegistry")
      file.mkdirs()
      val storage = new LSMStore(file, keySize = Address.addressSize)

      Some(new TokenBoxRegistry(storage, nodeKeys))

    } else None
  }
}
