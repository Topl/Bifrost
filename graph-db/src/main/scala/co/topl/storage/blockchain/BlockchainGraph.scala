package co.topl.storage.blockchain

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.storage.generic.SetStore
import co.topl.storage.graph._
import co.topl.storage.leveldb.LevelDBStore
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base58
import com.github.benmanes.caffeine.cache.Caffeine
import com.orientechnologies.orient.core.exception.OConcurrentModificationException
import org.mapdb.serializer.GroupSerializer
import org.mapdb.{DataInput2, DataOutput2, Serializer}
import scalacache.{Cache, _}
import scalacache.caffeine._
import scalacache.modes.scalaFuture._

import java.nio.charset.StandardCharsets
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions
import scala.jdk.CollectionConverters._

class BlockchainGraph(val graph: OrientDBGraph, parallelism: Int)(implicit
  system:                        ActorSystem[_],
  val genericStore:              LevelDBStore
) extends BlockchainData {

  import BlockchainGraph._
  import system.executionContext

  initialize()

  implicit private def opsProvider: BlockchainData = this

  private def stateStore(stateId: String): SetStore[String] =
    genericStore.forSet(stateId)

  import CacheConfig.defaultCacheConfig

  implicit private val blockStateModificationCache: Cache[BlockStateChange] =
    CaffeineCache(
      Caffeine.newBuilder
        .maximumSize(100_000L)
        .executor(system.executionContext)
        .build[String, Entry[BlockStateChange]]
    )

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      implicit private def readableGraph: ReadableGraph = graph

      override protected def instance: BlockHeader = header

      override def parentBlock: EitherT[Future, BlockchainData.Error, BlockHeader] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .out(BlockParent.edgeSchema)
          .single()

      override def childBlocks: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .in(BlockParent.edgeSchema)
          .source

      override def body: EitherT[Future, BlockchainData.Error, BlockBody] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .in(BodyHeader.edgeSchema)
          .single()

      override def history: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        Traverse(NodesByClass[BlockHeader](where = PropEquals("blockId", header.blockId)))
          .out(BlockParent.edgeSchema)
          .source
          .drop(1)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new GraphBlockBodyOps(body)

  private class GraphBlockBodyOps(body: BlockBody) extends BlockBodyOps {

    implicit private def readableGraph: ReadableGraph = graph

    override protected def instance: BlockBody = body

    override def header: EitherT[Future, BlockchainData.Error, BlockHeader] =
      Trace[BlockBody](where = PropEquals("blockId", body.blockId))
        .out(BodyHeader.edgeSchema)
        .single()

    override def transactions: Source[Either[BlockchainData.Error, Transaction], NotUsed] =
      Trace[BlockBody](where = PropEquals("blockId", body.blockId))
        .in(TransactionBlock.edgeSchema)
        .source

    override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box] = {
      import BlockchainGraphStateSupport.implicits._
      EitherT(
        Source
          .single(Right(body))
          .concat(body.traverseBlocksAndState)
          .takeWhile(_.isRight, inclusive = true)
          .mapAsync(parallelism) {
            case Right(body) =>
              body.stateChanges
                .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
                .flatMap(changes =>
                  if (changes.boxesOpened.contains(boxId))
                    Future.failed(BlockchainData.ErrorThrowable(BlockchainData.NotFound))
                  else if (changes.boxesCreated.contains(boxId))
                    boxId.box
                      .map(Some(_))
                      .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
                  else
                    Future.successful(None)
                )
            case Left(state) =>
              state
                .lookupUnopenedBox(boxId)
                .map(Some(_): Option[Box])
                .recover { case BlockchainData.NotFound => None }
                .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
          }
          .collect { case Some(box) => box }
          .runWith(Sink.headOption)
          .map(_.toRight(BlockchainData.NotFound))
          .recover {
            case e: BlockchainData.ErrorThrowable => Left(e.error)
            case _: NoSuchElementException        => Left(BlockchainData.NotFound)
            case e                                => Left(BlockchainData.ThrowableError(e))
          }
      )
    }

    override def stateChanges: EitherT[Future, BlockchainData.Error, BlockStateChange] =
      EitherT(
        cachingF(body.blockId)(ttl = None)(
          deriveStateChanges.valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
        )
          .map(Right(_))
          .recover {
            case BlockchainData.ErrorThrowable(e) => Left(e)
            case e                                => Left(BlockchainData.ThrowableError(e))
          }
      )

    private def deriveStateChanges: EitherT[Future, BlockchainData.Error, BlockStateChange] = {
      val query =
        s"""SELECT
           |  in('${TransactionBlock.edgeSchema.name}')
           |    .out('${TransactionBoxOpens.edgeSchema.name}')
           |    .boxId AS openedBoxIds,
           |  in('${TransactionBlock.edgeSchema.name}')
           |    .out('${TransactionBoxCreates.edgeSchema.name}')
           |    .boxId AS createdBoxIds
           | FROM ${BlockBody.nodeSchema.name}
           | WHERE blockId=?
           |""".stripMargin

      Raw[RawNode](query, Array[Any](body.blockId))
        .single()
        .map(raw =>
          BlockStateChange(
            raw.properties("openedBoxIds").asInstanceOf[java.util.List[String]].asScala.toSet,
            raw.properties("createdBoxIds").asInstanceOf[java.util.List[String]].asScala.toSet
          )
        )
    }

    override def state: EitherT[Future, BlockchainData.Error, State] =
      Trace[BlockBody](where = PropEquals("blockId", body.blockId))
        .out(BodyState.edgeSchema)
        .single()
  }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      implicit private def readableGraph: ReadableGraph = graph

      override protected def instance: Transaction = transaction

      override def blockBody: EitherT[Future, BlockchainData.Error, BlockBody] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBlock.edgeSchema)
          .single()

      override def opens: Source[Either[BlockchainData.Error, Box], NotUsed] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBoxOpens.edgeSchema)
          .source

      override def creates: Source[Either[BlockchainData.Error, Box], NotUsed] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBoxCreates.edgeSchema)
          .source
    }

  implicit override def boxOps(box: Box): BoxOps =
    new BoxOps {

      implicit private def readableGraph: ReadableGraph = graph

      override protected def instance: Box = box

      override def createdBy: EitherT[Future, BlockchainData.Error, Transaction] =
        Trace[Box](where = PropEquals("boxId", box.boxId))
          .in(TransactionBoxCreates.edgeSchema)
          .single()

      override def openedBy: Source[Either[BlockchainData.Error, Transaction], NotUsed] =
        Trace[Box](where = PropEquals("boxId", box.boxId))
          .in(TransactionBoxOpens.edgeSchema)
          .source
    }

  implicit override def stringOps(value: String): StringOps =
    new StringOps {

      implicit private def readableGraph: ReadableGraph = graph

      override protected def instance: String = value

      override def blockHeader: EitherT[Future, BlockchainData.Error, BlockHeader] =
        NodesByClass[BlockHeader](where = PropEquals("blockId", value)).single()

      override def blockBody: EitherT[Future, BlockchainData.Error, BlockBody] =
        NodesByClass[BlockBody](where = PropEquals("blockId", value)).single()

      override def transaction: EitherT[Future, BlockchainData.Error, Transaction] =
        NodesByClass[Transaction](where = PropEquals("transactionId", value)).single()

      override def box: EitherT[Future, BlockchainData.Error, Box] =
        NodesByClass[Box](where = PropEquals("boxId", value)).single()

      override def addressAccount: EitherT[Future, BlockchainData.Error, Account] =
        NodesByClass[Account](where = PropEquals("address", value)).single()

      /**
       * Retrieve the state associated with this String ID
       */
      override def state: EitherT[Future, BlockchainData.Error, State] =
        NodesByClass[State](where = PropEquals("stateId", value)).single()
    }

  implicit override def blockchainOps(blockchain: Blockchain.type): BlockchainOps =
    new BlockchainOps {

      implicit private def readableGraph: ReadableGraph = graph

      override def currentHead: EitherT[Future, BlockchainData.Error, BlockHeader] =
        Trace[ChainHead]().out(CanonicalHead.edgeSchema).single()

      override def currentHeads: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        NodesByClass[BlockHeader](where = PropEquals(s"in('${BlockParent.edgeSchema.name}').size()", 0)).source

      override def genesis: EitherT[Future, BlockchainData.Error, BlockHeader] =
        NodesByClass[BlockHeader](where = PropEquals(s"out('${BlockParent.edgeSchema.name}').size()", 0)).single()

      override def blocksAtHeight(height: Long): Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        NodesByClass[BlockHeader](where = PropEquals("height", height)).source
    }

  implicit override def stateOps(state: State): StateOps =
    new StateOps {
      override protected def instance: State = state

      override def unopenedBoxIds: Source[Either[BlockchainData.Error, String], NotUsed] =
        stateStore(state.stateId)
          .values()
          .map(Right(_))

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box] =
        stateStore(state.stateId)
          .contains(boxId)
          .leftMap(BlockchainData.GenericStoreError)
          .flatMap {
            case true => boxId.box
            case _    => EitherT.leftT[Future, Box](BlockchainData.NotFound)
          }
    }

  implicit override def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps =
    new BlockchainModificationsOps {

      /**
       * Run the chain modifications in-order
       */
      override def run(): EitherT[Future, BlockchainData.Error, Done] =
        EitherT(
          graph
            .transactionally { implicit session =>
              val support = new BlockchainGraphModificationSupport(parallelism)
              Source(modifications.toNonEmptyList.toList)
                .mapAsync(1) {
                  case c: CreateBlockHeader =>
                    support.apply(c).value
                  case c: CreateBlockBody =>
                    support.apply(c).value
                  case c: CreateTransaction =>
                    support.apply(c).value
                  case c: CreateBox =>
                    support.apply(c).value
                  case c: SetHead =>
                    support.apply(c).value
                  case c: AssociateBlockToParent =>
                    support.apply(c).value
                  case c: AssociateBodyToHeader =>
                    support.apply(c).value
                  case c: AssociateTransactionToBody =>
                    support.apply(c).value
                  case c: AssociateBoxCreator =>
                    support.apply(c).value
                  case c: AssociateBoxOpener =>
                    support.apply(c).value
                  case c: CreateState =>
                    support.apply(c).value
                }
                .takeWhile(_.isRight, inclusive = true)
                .runWith(Sink.last)
            }
            .recover { case _: OConcurrentModificationException => Left(BlockchainData.OrientDBConcurrencyError) }
        )
          .recoverWith { case BlockchainData.OrientDBConcurrencyError => run() }
    }

  private def initialize(): Unit =
    Await.result(
      graph
        .getNode[ChainHead](NodesByClass[ChainHead]())
        .valueOrF { case OrientDBGraph.ThrowableError(throwable) => Future.failed(throwable) }
        .flatMap {
          case Some(_) => Future.successful(Done)
          case _ =>
            graph.transactionally(
              _.insertNode(ChainHead())
                .valueOrF { case OrientDBGraph.ThrowableError(throwable) => Future.failed(throwable) }
            )
        },
      2.minutes
    )

  def close(): Unit = {
    graph.close()
    genericStore.close()
  }

}

object BlockchainGraph {

  implicit val stringCodec: LevelDBStore.BytesCodec[String] = new LevelDBStore.BytesCodec[String] {

    override def asBytes(v: String): Array[Byte] =
      v.getBytes(StandardCharsets.UTF_8)

    override def fromBytes(bytes: Array[Byte]): String = new String(bytes, StandardCharsets.UTF_8)
  }

  implicit class QueryOps[T: NodeSchema](query: GraphQuery[T]) {

    def single()(implicit graph: ReadableGraph, ec: ExecutionContext): EitherT[Future, BlockchainData.Error, T] =
      graph
        .getNode(query)
        .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
        .subflatMap(_.toRight(BlockchainData.NotFound))

    def source(implicit graph: ReadableGraph): Source[Either[BlockchainData.Error, T], NotUsed] =
      graph
        .getNodes(query)
        .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
        .takeWhile(_.isRight, inclusive = true)
  }

  implicit val stringSerializer: GroupSerializer[String] = Serializer.STRING

  implicit val blockStateChangeSerializer: Serializer[BlockStateChange] =
    new Serializer[BlockStateChange] {

      override def serialize(out: DataOutput2, value: BlockStateChange): Unit = {
        out.writeInt(value.boxesOpened.size)
        value.boxesOpened.foreach(out.writeUTF)
        out.writeInt(value.boxesCreated.size)
        value.boxesCreated.foreach(out.writeUTF)
      }

      override def deserialize(input: DataInput2, available: Int): BlockStateChange = {
        val boxesOpened = (0 until input.readInt()).map(_ => input.readUTF()).toSet
        val boxesCreated = (0 until input.readInt()).map(_ => input.readUTF()).toSet
        BlockStateChange(boxesOpened, boxesCreated)
      }
    }

  implicit val blockStateChangesCodec: LevelDBStore.BytesCodec[BlockStateChange] =
    new LevelDBStore.BytesCodec[BlockStateChange] {

      override def asBytes(v: BlockStateChange): Array[Byte] = {
        val out = new DataOutput2
        blockStateChangeSerializer.serialize(out, v)
        out.copyBytes()
      }

      override def fromBytes(bytes: Array[Byte]): BlockStateChange = {
        val in = new DataInput2.ByteArray(bytes)
        blockStateChangeSerializer.deserialize(in, 0)
      }
    }
}

object BlockchainGraphStateSupport {

  trait Ops {

    implicit class BlockBodyStateOps(blockBody: BlockBody) {

      /**
       * Does not include the current `blockBody`
       */
      private[blockchain] def traverseBlocksAndState(implicit
        opsProvider:   BlockchainData,
        readableGraph: ReadableGraph,
        mat:           Materializer
      ): Source[Either[State, BlockBody], NotUsed] = {
        import BlockchainGraph._
        import mat.executionContext
        Source
          .unfoldAsync(blockBody) { body =>
            val traversalFields = List(
              s"out('${BodyState.edgeSchema.name}')",
              s"out('${BodyHeader.edgeSchema.name}')",
              s"out('${BlockParent.edgeSchema.name}')",
              s"in('${BodyHeader.edgeSchema.name}')"
            )
            val targetQuery = s"SELECT FROM ${BlockBody.nodeSchema.name} WHERE blockId=? LIMIT 1"
            val traverseQuery =
              s"TRAVERSE ${traversalFields.mkString(",")}" +
              s" FROM ($targetQuery)" +
              " LIMIT 50" +
              " strategy BREADTH_FIRST"
            val query =
              s"SELECT FROM ($traverseQuery)" +
              s" WHERE (@class='${BlockBody.nodeSchema.name}') OR (@class='${State.nodeSchema.name}')"
            Raw[RawNode](query, Array[Any](body.blockId)).source
              .map(_.getOrThrow(BlockchainData.ErrorThrowable))
              .drop(1)
              .map {
                case raw if raw.className == BlockBody.nodeSchema.name =>
                  Right(BlockBody.nodeSchema.decode(raw.properties))
                case raw if raw.className == State.nodeSchema.name =>
                  Left(State.nodeSchema.decode(raw.properties))
                case raw =>
                  throw new IllegalArgumentException(s"Unexpected traversal node returned. $raw")
              }
              .runWith(Sink.seq)
              .map {
                case Nil => None
                case values =>
                  values
                    .findLast(_.isRight)
                    .map { case Right(b) =>
                      b -> values
                    }
              }
          }
          .mapConcat(identity)
      }

      def createState(parallelism: Int)(implicit
        opsProvider:               BlockchainData,
        ec:                        ExecutionContext,
        readableGraph:             ReadableGraph,
        writableGraph:             WritableGraph,
        mat:                       Materializer,
        genericStore:              LevelDBStore
      ): EitherT[Future, BlockchainData.Error, Done] = {
        import opsProvider._
        blockBody.state
          .map(_ => Done)
          .recoverWith { case BlockchainData.NotFound =>
            EitherT(
              (for {
                blockStateHistory <-
                  traverseBlocksAndState
                    .takeWhile(_.isRight, inclusive = true)
                    .runWith(Sink.seq)
                    .map(_.reverse :+ Right(blockBody))
                blockStateChangeHistory <-
                  Source(blockStateHistory.collect { case Right(b) => b })
                    .mapAsync(parallelism)(b =>
                      b.stateChanges
                        .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
                    )
                    .runWith(Sink.seq)
                closestState = blockStateHistory.collectFirst { case Left(state) => state }
                historyAfterClosestState =
                  blockStateChangeHistory.drop(closestState.size.toInt)
                encodedStateId = Base58.encode(
                  historyAfterClosestState
                    .map(_.hash)
                    .foldLeft(
                      closestState.fold(Array.fill(32)(0: Byte))(cs => Base58.decode(cs.stateId).getOrThrow())
                    ) { case (previousStateHash, changeHistoryHash) =>
                      co.topl.crypto.hash.blake2b256.hash(None, previousStateHash, changeHistoryHash).value
                    }
                )
                _ <- writableGraph
                  .insertNode[State](State(encodedStateId))
                  .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(BlockchainData.OrientDBGraphError(e))))
                mergedStateChange =
                  historyAfterClosestState.fold(BlockStateChange(Set.empty, Set.empty))(_.combine(_))
                knownUnopenedBoxes =
                  mergedStateChange.boxesCreated -- mergedStateChange.boxesOpened
                knownOpenedBoxes = mergedStateChange.boxesOpened
                _ <- associateBodyToState(encodedStateId)
                _ <- insertStateUnopenedBoxEdges(closestState, knownUnopenedBoxes, knownOpenedBoxes, encodedStateId)
              } yield Done)
                .map(Right(_))
                .recover { case e => Left(BlockchainData.ThrowableError(e)) }
            )
          }
      }
        .map(_ => Done)

      private def associateBodyToState(
        encodedStateId: String
      )(implicit ec:    ExecutionContext, orientDBGraph: WritableGraph) =
        orientDBGraph
          .insertEdge(
            BodyState(),
            OrientDBGraph.NodeReference(
              NodesByClass[BlockBody](where = PropEquals("blockId", blockBody.blockId))
            ),
            OrientDBGraph.NodeReference(
              NodesByClass[State](where = PropEquals("stateId", encodedStateId))
            )
          )
          .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(BlockchainData.OrientDBGraphError(e))))

      private def insertStateUnopenedBoxEdges(
        closestState:       Option[State],
        knownUnopenedBoxes: Set[String],
        knownOpenedBoxes:   Set[String],
        encodedStateId:     String
      )(implicit
        opsProvider:   BlockchainData,
        orientDBGraph: WritableGraph,
        mat:           Materializer,
        genericStore:  LevelDBStore
      ) = {
        import BlockchainGraph._
        import opsProvider._
        val setStore = genericStore.forSet[String](encodedStateId)
        Source(knownUnopenedBoxes)
          .concat(
            closestState.fold(Source.empty[String])(
              _.unopenedBoxIds.map(_.getOrThrow()).filterNot(knownOpenedBoxes.contains)
            )
          )
          .runWith(setStore.putMany())
      }
    }
  }

  object implicits extends Ops
}

class BlockchainGraphModificationSupport(parallelism: Int)(implicit
  blockchainData:                                     BlockchainData,
  readableGraph:                                      ReadableGraph,
  writableGraph:                                      WritableGraph,
  executionContext:                                   ExecutionContext,
  materializer:                                       Materializer,
  genericStore:                                       LevelDBStore
) {
  import blockchainData._

  def apply(createState: CreateState): EitherT[Future, BlockchainData.Error, Done] = {
    import BlockchainGraphStateSupport.implicits._
    createState.blockId.blockBody
      .flatMap(_.createState(parallelism))
  }

  def apply(createBlockHeader: CreateBlockHeader): EitherT[Future, BlockchainData.Error, Done] =
    writableGraph.insertNode(createBlockHeader.header).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createBlockBody: CreateBlockBody): EitherT[Future, BlockchainData.Error, Done] =
    writableGraph.insertNode(createBlockBody.body).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createTransaction: CreateTransaction): EitherT[Future, BlockchainData.Error, Done] =
    writableGraph.insertNode(createTransaction.transaction).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createBox: CreateBox): EitherT[Future, BlockchainData.Error, Done] =
    writableGraph.insertNode(createBox.box).leftMap(BlockchainData.OrientDBGraphError)

  def apply(setHead: SetHead): EitherT[Future, BlockchainData.Error, Done] =
    writableGraph
      .deleteEdges[CanonicalHead]()
      .flatMap(_ =>
        writableGraph.insertEdge(
          CanonicalHead(),
          OrientDBGraph.NodeReference(NodesByClass[ChainHead]()),
          OrientDBGraph.NodeReference(NodesByClass[BlockHeader](where = PropEquals("blockId", setHead.blockId)))
        )
      )
      .leftMap(BlockchainData.OrientDBGraphError)

  def apply(associateBlockToParent: AssociateBlockToParent): EitherT[Future, BlockchainData.Error, Done] = {
    val parentHeaderReference =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.parentBlockId))
      )
    val childHeader =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.childBlockId))
      )

    writableGraph
      .insertEdge(BlockParent(), childHeader, parentHeaderReference)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBodyToHeader: AssociateBodyToHeader): EitherT[Future, BlockchainData.Error, Done] = {
    val headerId = associateBodyToHeader.headerId
    val bodyId = associateBodyToHeader.bodyId
    val header =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", headerId))
      )
    val body =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockBody](where = PropEquals("blockId", bodyId))
      )
    writableGraph
      .insertEdge(BodyHeader(), body, header)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateTransactionToBody: AssociateTransactionToBody): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateTransactionToBody.transactionId
    val blockId = associateTransactionToBody.blockId
    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val body =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockBody](where = PropEquals("blockId", blockId))
      )
    writableGraph
      .insertEdge(TransactionBlock(), transaction, body)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBoxCreator: AssociateBoxCreator): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateBoxCreator.transactionId
    val boxId = associateBoxCreator.boxId

    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val box =
      OrientDBGraph.NodeReference(
        NodesByClass[Box](where = PropEquals("boxId", boxId))
      )
    writableGraph
      .insertEdge(TransactionBoxCreates(minted = true), transaction, box)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBoxOpener: AssociateBoxOpener): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateBoxOpener.transactionId
    val boxId = associateBoxOpener.boxId

    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val box =
      OrientDBGraph.NodeReference(
        NodesByClass[Box](where = PropEquals("boxId", boxId))
      )
    writableGraph
      .insertEdge(TransactionBoxOpens(), transaction, box)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

}
