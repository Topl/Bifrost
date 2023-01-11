package co.topl.genusLibrary.algebras

import co.topl.genusLibrary.Txo
import co.topl.genusLibrary.failure.Failure
import co.topl.genusLibrary.model.BlockData
import co.topl.genusLibrary.orientDb.wrapper.GraphTxWrapper
import co.topl.models.{BlockBody, BlockHeader, Transaction}
import co.topl.proto.models.TypedEvidence

/**
 * Mediator of content inserted to a data store. Based on the Mediator software design pattern.
 * For more information, refer to <a href="https://en.wikipedia.org/wiki/Mediator_pattern">Mediator pattern</a>
 * @tparam F the effect-ful context to retrieve the value in
 */
trait Mediator[F[_]] {

  /**
   * Mediate after insertion of header. Commits DB transaction.
   *
   * @param block the full block data for context reasons
   * @param graph graph in which to commit transaction
   * @return Unit
   */
  def afterHeaderInserted(block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of body. Commits DB transaction.
   *
   * @param body inserted block body
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterBodyInserted(body: BlockBody, block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of transaction. Commits DB transaction.
   *
   * @param transaction inserted transaction
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterTxInserted(transaction: Transaction, block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of address. Commits DB transaction.
   *
   * @param address inserted address
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterAddressInserted(address: TypedEvidence, block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of transaction output. Commits DB transaction.
   *
   * @param txo inserted transaction output
   * @param block  the full block data for context reasons
   * @param graph  graph in which to commit transaction
   * @return Unit
   */
  def afterTxoInserted(txo: Txo, block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

  /**
   * Mediate after insertion of address state. Commits DB transaction.
   *
   * @param block the full block data for context reasons
   * @param graph in which to commit transaction
   * @return Unit
   */
  def afterAddressStateInserted(block: BlockData, graph: GraphTxWrapper[F]): F[Either[Failure, Unit]]

}
