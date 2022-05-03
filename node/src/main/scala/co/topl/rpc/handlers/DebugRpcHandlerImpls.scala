package co.topl.rpc.handlers

import akka.actor.typed.ActorSystem
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.codecs._
import co.topl.consensus.{KeyManagerInterface, ListOpenKeyfilesFailureException}
import co.topl.modifier.block.Block
import co.topl.nodeView.history.HistoryDebug
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType.{NetworkPrefix, pickNetworkType}
import co.topl.utils.encode.Base58
import io.circe.Encoder
import io.circe.syntax.EncoderOps
import scodec.Codec

import java.io.{BufferedWriter, FileWriter}
import scala.util.Try

class DebugRpcHandlerImpls(
  nodeViewHolderInterface: NodeViewHolderInterface,
  keyManagerInterface:     KeyManagerInterface
)(implicit
  system:           ActorSystem[_],
  throwableEncoder: Encoder[ThrowableData],
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Debug {

  import system.executionContext

  override val delay: ToplRpc.Debug.Delay.rpc.ServerHandler =
    params =>
      withNodeView(view => new HistoryDebug(view.history).averageDelay(params.blockId, params.numBlocks))
        .subflatMap(
          _.toEither
            .leftMap(CustomError.fromThrowable(_): RpcError)
        )
        .map(delay => ToplRpc.Debug.Delay.Response(s"$delay milliseconds"))

  override val myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler =
    _ =>
      for {
        myKeys <- keyManagerInterface.listOpenKeyfiles().leftMap { case ListOpenKeyfilesFailureException(throwable) =>
          CustomError.fromThrowable(throwable): RpcError
        }
        blockNum <- withNodeView(view =>
          new HistoryDebug(view.history).count(b => myKeys.map(_.evidence).contains(b.generatorBox.evidence))
        )
      } yield ToplRpc.Debug.MyBlocks.Response(myKeys, blockNum)

  override val generators: ToplRpc.Debug.Generators.rpc.ServerHandler =
    _ =>
      withNodeView(view => new HistoryDebug(view.history).forgerDistribution())
        .map(
          _.map { case (key, count) =>
            key.address -> count
          }
        )

  override val idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler =
    params =>
      withNodeView(view =>
        new HistoryDebug(view.history)
          .getIdsFrom(params.height, params.limit)
      )
        .subflatMap(
          _.toRight(ToplRpcErrors.NoBlockIdsAtHeight: RpcError)
            .map(_.toList)
        )

  override val exportGenesisAndKeys: ToplRpc.Debug.ExportGenesisAndKeys.rpc.ServerHandler =
    params =>
      for {
//        path <- EitherT.pure[Future, RpcError](new File(params.path))
//        _    <- EitherT.pure[Future, RpcError](path.mkdirs())
//        addresses <- keyManagerInterface
//          .exportOpenKeyfiles(params.passwords, path.getAbsolutePath)
//          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)

        block <- withNodeView(_.history.modifierByHeight(1)).subflatMap(_.toRight(ToplRpcErrors.NoBlockAtHeight))

//        _ <- Future
//          .fromTry(saveBlockJsonToDisk(block, path.getAbsolutePath))
//          .attemptT
//          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)

//        blockBytes <- EitherT.pure[Future, RpcError](Persistable[NodeViewModifier].persistedBytes(block))

        secrets <- keyManagerInterface.getOpenKeys
          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)

//        _ <- Future
//          .fromTry(saveBlockAndSecretBytesToDisk(blockBytes, secretBytes, path.getAbsolutePath))
//          .attemptT
//          .leftMap(e => ToplRpcErrors.genericFailure(e.toString): RpcError)

      } yield {

        val secretList = secrets.flatMap {
          case s: PrivateKeyCurve25519 => Some(s)
          case _                       => None
        }.toList

        val encodedPrivateKeys =
          Codec[List[PrivateKeyCurve25519]]
            .encode(secretList)
            .getOrElse(throw new Exception("Unable to encode the list of private keys"))
            .toBase58

        val encodedBlock =
          Codec[Block].encode(block).getOrElse(throw new Exception("Unable to encode the block")).toBase58


        ToplRpc.Debug.ExportGenesisAndKeys.Response(secretList, block)
      }

  private def saveBlockJsonToDisk(block: Block, path: String): Try[Unit] = Try {
    val networkName = pickNetworkType(networkPrefix).get.verboseName
    val w = new BufferedWriter(new FileWriter(s"$path/$networkName-genesis.json"))
    w.write(block.asJson.toString)
    w.close()
  }

  private def saveBlockAndSecretBytesToDisk(
    blockBytes:   Array[Byte],
    secretsBytes: Array[Byte],
    path:         String
  ): Try[Unit] = Try {
    val networkName = pickNetworkType(networkPrefix).get.verboseName
    val w = new BufferedWriter(new FileWriter(s"$path/$networkName-genesis.txt"))
    w.write(Base58.encode(blockBytes))
    w.write("\n=============================\n")
    w.write(Base58.encode(secretsBytes))
    w.close()
  }

  private def withNodeView[T](f: ReadableNodeView => T) =
    nodeViewHolderInterface
      .withNodeView(f)
      .leftMap { case NodeViewHolderInterface.ReadFailure(throwable) =>
        CustomError.fromThrowable(throwable): RpcError
      }
}
