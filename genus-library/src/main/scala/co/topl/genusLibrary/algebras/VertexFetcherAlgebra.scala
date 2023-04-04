package co.topl.genusLibrary.algebras

import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId
import co.topl.genusLibrary.model.GE
import com.tinkerpop.blueprints.Vertex

/**
 * Vertex finder on the stored Ledger
 * @tparam F the effect-ful context to retrieve the value in
 */
trait VertexFetcherAlgebra[F[_]] {

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
   * @param headerVertex filter by field
   * @return transactions vertices
   */
  def fetchTransactions(headerVertex: Vertex): F[Either[GE, Iterable[Vertex]]]

  /**
   * Fetch Transaction Vertex, using TransactionIndex
   *
   * @param ioTransaction32 filter by index field
   * @return Optional transaction vertex, None if it was not found
   */
  def fetchTransaction(ioTransaction32: Identifier.IoTransaction32): F[Either[GE, Option[Vertex]]]

  /**
   * Fetch Address Vertex, using Address Inex
   *
   * @param addressId filter by index field
   * @return Optional Address vertex, None if it was not found
   */
  def fetchAddress(addressId: Identifier): F[Either[GE, Option[Vertex]]]

}
