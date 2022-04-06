package co.topl.interpreters

import cats.effect.Sync
import co.topl.eventtree.ParentChildTree
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._

object BlockIdTree {

  // TODO: Use LevelDB
  def make[F[_]: Sync]: F[ParentChildTree[F, TypedIdentifier]] =
    ParentChildTree.FromRef.make[F, TypedIdentifier]
}
