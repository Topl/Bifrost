package co.topl.genus.algebras

import co.topl.brambl.models.{GroupId, LockAddress, SeriesId, TransactionId, TransactionOutputAddress}
import co.topl.consensus.models.BlockId
import co.topl.genus.model.GE
import co.topl.genus.services.{BlockStats, BlockchainSizeStats, TxoState, TxoStats}
import com.tinkerpop.blueprints.Vertex

/**
 * Vertex finder on the stored Ledger
 * @tparam F the effect-ful context to retrieve the value in
 */
trait VertexFetcherAlgebra[F[_]] {

  /**
   * Fetch Canonical head vertex on the stored Ledger
   *
   * @return Optional header vertex, None if it was not found
   */
  def fetchCanonicalHead(): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch a BlockHeader vertex on the stored Ledger
   * @param blockId  blockId filter by field
   * @return Optional header vertex, None if it was not found
   */
  def fetchHeader(blockId: BlockId): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch a BlockHeader vertex on the stored Ledger
   *
   * @param height filter by field
   * @return Optional header vertex, None if it was not found
   */
  def fetchHeaderByHeight(height: Long): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch a BlockHeader vertex on the stored Ledger
   *
   * @param depth filter by field, The block at depth 1 is the highest block
   * @return Optional header vertex, None if it was not found
   */
  def fetchHeaderByDepth(depth: Long): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch a BlockBody Vertex, which depends on header Vertex the stored Ledger, using the link to BlockHeader defined in the schema
   *
   * @param headerVertex filter by field
   * @return Optional body vertex, None if it was not found
   */
  def fetchBody(headerVertex: Vertex): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Transactions Vertices, which depends on header Vertex the stored Ledger, using the link to BlockHeader defined in the schema
   *
   * Does not include the reward transaction
   *
   * @param headerVertex filter by field
   * @return transactions vertices
   */
  def fetchTransactions(headerVertex: Vertex): F[Either[GE, List[Vertex]]]

  /**
   * Fetch the optional "Reward" Transaction Vertex associated with the given BlockHeader
   *
   * @param headerVertex filter by field
   * @return optional transactions vertex
   */
  def fetchRewardTransaction(headerVertex: Vertex): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Transaction Vertex, using TransactionIndex
   *
   * @param ioTransaction32 filter by index field
   * @return Optional transaction vertex, None if it was not found
   */
  def fetchTransaction(transactionId: TransactionId): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch LockAddress Vertex, using Address Index
   *
   * @param lockAddress filter by index field
   * @return Optional Address vertex, None if it was not found
   */
  def fetchLockAddress(lockAddress: LockAddress): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Txo Vertex, using TxoIndex (outputAddress.id + outputAddress.index)
   * @param transactionOutputAddress
   * @return Optional Txo vertex, None if it was not found
   */
  def fetchTxo(transactionOutputAddress: TransactionOutputAddress): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Txo Vertices associated with the given LockAddress
   * @param lockAddressVertex the LockAddress vertex which contains edges to TxO vertices
   * @return Txo vertices
   */
  def fetchTxosByLockAddress(lockAddressVertex: Vertex, state: TxoState): F[Either[GE, List[Vertex]]]

  /**
   * Fetch Txo Stats
   *
   * @return a vertex with stats TxOs spent, unspent ...
   */
  def fetchTxoStats(): F[Either[GE, TxoStats]]

  def fetchBlockchainSizeStats(): F[Either[GE, BlockchainSizeStats]]

  /**
   * Fetch Block Stats
   * Returns total blocks with and without transactions in them
   */
  def fetchBlockStats(): F[Either[GE, BlockStats]]

  /**
   * Fetch Group Policy vertex
   * @param groupId groupId
   * @return a group policy
   */
  def fetchGroupPolicy(groupId: GroupId): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Series Policy vertex
   *
   * @param seriesId series Id
   * @return a series policy
   */
  def fetchSeriesPolicy(seriesId: SeriesId): F[Either[GE, Option[Vertex]]]
}
