package co.topl.interpreters

import cats.effect.Async
import co.topl.eventtree.ParentChildTree
import co.topl.models.TypedIdentifier
import co.topl.typeclasses.implicits._

object BlockIdTree {

  // TODO: Use LevelDB
  def make[F[_]: Async]: F[ParentChildTree[F, TypedIdentifier]] =
    ParentChildTree.FromSemaphore.make[F, TypedIdentifier]
}
