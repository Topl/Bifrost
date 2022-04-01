package co.topl.consensus

import cats.data.EitherT
import cats.implicits._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class MockConsensusReader(view: NxtConsensus.View)(implicit executionContext: ExecutionContext)
    extends ConsensusReader {

  override def withView[T](f: NxtConsensus.View => T): EitherT[Future, ConsensusInterface.WithViewFailure, T] =
    EitherT.right[ConsensusInterface.WithViewFailure](
      Future.fromTry(Try(f(view)))
    )

  override def readState: EitherT[Future, ConsensusInterface.ReadStateFailure, NxtConsensus.State] =
    EitherT.right[ConsensusInterface.ReadStateFailure](
      Future.successful(view.state)
    )
}

object MockConsensusReader {

  def apply(view: NxtConsensus.View)(implicit executionContext: ExecutionContext): ConsensusReader =
    new MockConsensusReader(view)
}
