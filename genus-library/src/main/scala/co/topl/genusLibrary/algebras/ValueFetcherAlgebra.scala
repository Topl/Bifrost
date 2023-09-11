package co.topl.genusLibrary.algebras

import co.topl.brambl.models.Event.GroupPolicy
import co.topl.brambl.models.GroupId
import co.topl.genusLibrary.model.GE

/**
 * Fetch Value information stored the data store.
 * Value: LVL, TOPL, Asset, Group, Series,..
 * @tparam F the effect-ful context to retrieve the value in
 */
trait ValueFetcherAlgebra[F[_]] {

  /**
   * Fetch Group Policy
   *
   * @param groupId groupId
   * @return a group policy
   */
  def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[GroupPolicy]]]

}
