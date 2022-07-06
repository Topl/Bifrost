package co.topl.consensus

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class MockConsensusReader(state: NxtConsensus.State)(implicit executionContext: ExecutionContext)
    extends ConsensusReader {

  override def lookupState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State] =
    EitherT.right[ConsensusInterface.ReadStateFailure](
      Future.successful(state)
    )
}

object MockConsensusReader {

  def apply(state: NxtConsensus.State)(implicit executionContext: ExecutionContext): ConsensusReader =
    new MockConsensusReader(state)
}
