package co.topl.consensus

import akka.util.Timeout
import cats.data.{NonEmptySet, OptionT}
import cats.effect.Async
import cats.kernel.Order
import cats.implicits._
import co.topl.models.{SlotId, TypedIdentifier}
import co.topl.typeclasses.implicits._

import scala.collection.immutable.SortedSet

trait TinePool[F[_]] {

  /**
   * The current "tips" of all chains.
   */
  def heads: F[NonEmptySet[CandidateTine[F]]]

  /**
   * The SlotId currently referenced by this TinePool's [[DeepestCommonAncestor]]
   */
  def deepestCommonAncestor: F[SlotId]

  /**
   * Instruct the TinePool to include the (already validated) Slot ID
   */
  def appendTine(slotId: SlotId, parentSlotId: SlotId): F[Unit]

  /**
   * Tell the tine pool that the local node has received the block at the given slotId from a peer.  If the TinePool
   * is already aware of the given slotId, `Ignored` is returned.  If the given slotId leads to a known tine, `JobComplete`
   * is returned.  Otherwise, the TinePool will `RequestAnother` block (the given parentSlotId).
   */
  def appendJob(slotId: SlotId, parentSlotId: SlotId): F[TinePool.AppendJobResult]

  /**
   * Retrieve the Tine associated with the given blockId, or None if the blockId is not known in any Tine.
   *
   * If the blockId is not known in a Tine, it may still be known in a Job.
   */
  def fetchTine(blockId: TypedIdentifier): F[Option[CandidateTine[F]]]

  /**
   * Retrieve the Job associated with the given blockId, or None if the blockId is not known in any Job.
   */
  def fetchJob(blockId: TypedIdentifier): F[Option[OrphanTine[F]]]
}

object TinePool {
  abstract class AppendJobResult

  object AppendJobResults {

    /**
     * Indicates that the provided job is already known
     */
    case object Ignored extends AppendJobResult

    /**
     * Indicates that a job was properly appended, but the tine pool requires further blocks in order to find a path
     * to the local chain
     * @param slotId The SlotId that is needed to continue processing
     */
    case class RequestAnother(slotId: SlotId) extends AppendJobResult

    /**
     * Indicates that a job was completed which finally found its way back to a block in the node's local chain.
     * @param jobEntries The corresponding SlotIds of all n-child blocks of the completed job
     */
    case class JobComplete(jobEntries: Map[SlotId, SlotId]) extends AppendJobResult
  }
}

object TinePoolActor {

  import akka.actor.typed._
  import akka.actor.typed.scaladsl._

  case class State(
    canonicalReference: SlotId,
    tineParents:        Map[SlotId, SlotId],
    jobParents:         Map[SlotId, SlotId]
  )

  case class Run[Res](f: State => (State, Res), replyTo: ActorRef[Res]) {

    private[TinePoolActor] def run(state: State): State = {
      val (newState, res) = f(state)
      replyTo.tell(res)
      newState
    }
  }

  def apply(state: State): Behavior[Run[_]] =
    Behaviors.receiveMessage(r => apply(r.run(state)))

  object Eval {

    import akka.actor.typed.scaladsl.AskPattern._

    def make[F[_]: Async](actorRef: ActorRef[Run[_]])(implicit system: ActorSystem[_], timeout: Timeout): TinePool[F] =
      new TinePool[F] {
        implicit val orderTines: Order[CandidateTine[F]] = Order.allEqual[CandidateTine[F]]

        def heads: F[NonEmptySet[CandidateTine[F]]] =
          withState(state => state -> state)
            .map { state =>
              val tineParents = state.tineParents
              val keySet = tineParents.keySet
              val valueSet = tineParents.values.toSet
              val leafs = keySet -- valueSet
              if (leafs.isEmpty) NonEmptySet.one(DeepestCommonAncestor(state.canonicalReference))
              else
                NonEmptySet
                  .fromSet(
                    SortedSet.from[CandidateTine[F]](
                      leafs.map(slotId =>
                        BlockCandidateTine(
                          slotId,
                          if (slotId === state.canonicalReference)
                            (DeepestCommonAncestor(slotId): CandidateTine[F]).pure[F]
                          else
                            OptionT(fetchTine(tineParents(slotId)._2)).getOrElseF(
                              new IllegalStateException(s"Expected tineId=${tineParents(slotId)._2} to exist")
                                .raiseError[F, CandidateTine[F]]
                            )
                        )
                      )
                    )(orderTines.toOrdering)
                  )
                  .getOrElse(NonEmptySet.one(DeepestCommonAncestor(state.canonicalReference)))
            }

        def deepestCommonAncestor: F[SlotId] =
          withState(state => state -> state.canonicalReference)

        def appendTine(slotId: SlotId, parentSlotId: SlotId): F[Unit] =
          withState(state => state.copy(tineParents = state.tineParents.updated(slotId, parentSlotId)) -> ())

        def appendJob(
          slotId:       SlotId,
          parentSlotId: SlotId
        ): F[TinePool.AppendJobResult] = withState { state =>
          if (state.tineParents.contains(parentSlotId)) {
            var acc: Map[SlotId, SlotId] = Map.empty
            var result: Map[SlotId, SlotId] = state.jobParents
            var buffer: List[SlotId] = List(slotId)
            while (buffer.nonEmpty) {
              val slotId = buffer.head
              buffer = buffer.tail
              val (matches, newResult) = result.partition(_._2 == slotId)
              acc ++= matches
              buffer ++= matches.keys
              result = newResult
            }
            state.copy(jobParents = result) -> TinePool.AppendJobResults.JobComplete(acc)
          } else if (state.jobParents.contains(slotId)) {
            state -> TinePool.AppendJobResults.Ignored
          } else {
            state.copy(tineParents = state.tineParents.updated(slotId, parentSlotId)) -> TinePool.AppendJobResults
              .RequestAnother(parentSlotId)
          }
        }

        def fetchTine(blockId: TypedIdentifier): F[Option[CandidateTine[F]]] =
          withState(state => state -> state)
            .flatMap(state =>
              Async[F].delay(
                state.tineParents
                  .collectFirst {
                    case (slotId, parentSlotId) if slotId._2 === blockId =>
                      if (parentSlotId === state.canonicalReference) DeepestCommonAncestor[F](state.canonicalReference)
                      else
                        BlockCandidateTine(
                          slotId,
                          OptionT(fetchTine(parentSlotId._2)).getOrElseF(
                            new IllegalStateException(s"Expected tineId=$parentSlotId to exist")
                              .raiseError[F, CandidateTine[F]]
                          )
                        )
                  }
              )
            )

        def fetchJob(blockId: TypedIdentifier): F[Option[OrphanTine[F]]] =
          withState(state => state -> state.jobParents)
            .flatMap(parents =>
              Async[F].delay(
                parents
                  .collectFirst {
                    case (slotId, parentSlotId) if slotId._2 === blockId =>
                      BlockOrphanTine(
                        slotId,
                        OptionT(fetchJob(parentSlotId._2)).getOrElseF(
                          new IllegalStateException(s"Expected jobId=$parentSlotId to exist")
                            .raiseError[F, OrphanTine[F]]
                        )
                      )
                  }
              )
            )

        private def withState[Res](f: State => (State, Res)): F[Res] =
          Async[F].fromFuture(
            Async[F].delay(actorRef.ask[Res](Run(f, _)))
          )
      }
  }

}
