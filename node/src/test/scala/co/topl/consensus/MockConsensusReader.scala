package co.topl.consensus

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class MockConsensusReader(state: ConsensusHolder.State)(implicit executionContext: ExecutionContext)
    extends ConsensusReader {

  override def lookupState: EitherT[Future, ConsensusHolderInterface.ReadStateFailure, ConsensusHolder.State] =
    EitherT.right[ConsensusHolderInterface.ReadStateFailure](
      Future.successful(state)
    )
}

object MockConsensusReader {

  def apply(state: ConsensusHolder.State)(implicit executionContext: ExecutionContext): ConsensusReader =
    new MockConsensusReader(state)
}
