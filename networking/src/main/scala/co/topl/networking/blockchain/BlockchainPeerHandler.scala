package co.topl.networking.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Flow, Keep, RunnableGraph, Sink, Source}
import cats.data._
import cats.effect.kernel.Sync
import cats.effect.{Async, Concurrent, Resource}
import cats.implicits._
import cats.{~>, Monad, MonadThrow, Parallel}
import co.topl.algebras.{Store, StoreReader}
import co.topl.catsakka._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.consensus.algebras.LocalChainAlgebra
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.ledger.algebras.{MempoolAlgebra, TransactionSyntaxValidationAlgebra}
import co.topl.models._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

trait BlockchainPeerHandler[F[_]] {
  def usePeer(client: BlockchainPeerClient[F]): F[Unit]
}

