package co.topl.genusLibrary.orientDb

import cats.effect.kernel.Async
import co.topl.genusLibrary.orientDb.wrapper.WrappedVertex
import com.tinkerpop.blueprints.impls.orient.OrientGraphNoTx

private[genusLibrary] trait DBFacade {

  type VertexTypeName = String

  type PropertyKey = String
  type PropertyQuery = (PropertyKey, AnyRef)

  /**
   * Shut down the OrientDB server.
   *
   * @return true if the server was running and got shut down
   */
  def shutdown(): Boolean

  def getGraph[F[_]: Async: org.typelevel.log4cats.Logger]: GraphTxDAO[F]

  def getGraphNoTx: OrientGraphNoTx

  // TODO Unify VertexTypeName and PropertyKey with VertexSchema (VertexSchema.BlockHeader.BlockId)
  /**
   * Get single vertex filtered by only one field
   *
   * @param vertexTypeName Vertex class
   * @param filterKey      Vertex key to filter by
   * @param filterValue    Vertex value of given key to filter by
   * @tparam F the effect-ful context to retrieve the value in
   * @return Optional Vertex
   */
  def getVertexByField[F[_]: Async](
    vertexTypeName: VertexTypeName,
    filterKey:      PropertyKey,
    filterValue:    AnyRef
  ): F[Option[WrappedVertex]]

  /**
   * Get single vertex filtered by multiple properties
   *
   * @param vertexTypeName   Vertex class
   * @param propertiesFilter Vertex properties to filter by
   * @tparam F the effect-ful context to retrieve the value in
   * @return Optional Vertex
   */
  def getVertexByFields[F[_]: Async](
    vertexTypeName:   VertexTypeName,
    propertiesFilter: Set[PropertyQuery]
  ): F[Option[WrappedVertex]]

  /**
   * Get vertices filtered by only one field
   *
   * @param vertexTypeName Vertices class
   * @param filterKey      Vertices key to filter by
   * @param filterValue    Vertices value of given key to filter by
   * @tparam F the effect-ful context to retrieve the value in
   * @return Vertices
   */
  def getVerticesByField[F[_]: Async](
    vertexTypeName: VertexTypeName,
    filterKey:      PropertyKey,
    filterValue:    AnyRef
  ): F[Iterable[WrappedVertex]]

  /**
   * Get vertices filtered by multiple properties
   *
   * @param vertexTypeName   Vertex class
   * @param propertiesFilter Vertices properties to filter by
   * @tparam F the effect-ful context to retrieve the value in
   * @return Vertices
   */
  def getVerticesByFields[F[_]: Async](
    vertexTypeName:   VertexTypeName,
    propertiesFilter: Set[PropertyQuery]
  ): F[Iterable[WrappedVertex]]
}
