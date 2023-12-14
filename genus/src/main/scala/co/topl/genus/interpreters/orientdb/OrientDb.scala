package co.topl.genus.interpreters.orientdb

import cats.Traverse
import cats.data.Chain
import cats.implicits._
import cats.effect.Sync
import cats.effect.kernel.Resource
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient.{
  OrientBaseGraph,
  OrientDynaElementIterable,
  OrientElement,
  OrientGraphFactory,
  OrientVertex
}

object OrientDb {

  private val Password = "topl"

  def inMemory[F[_]: Sync]: Resource[F, OrientGraphFactory] =
    Resource.make(
      for {
        factory <- Sync[F].delay(new OrientGraphFactory("memory:blockchain", "admin", "admin"))
        session <- Sync[F].blocking(factory.getNoTx)
        _ <- Sync[F].blocking(
          session.command(new OCommandSQL(s"UPDATE OUser SET password='$Password' WHERE name='admin'"))
        )
        _ <- Sync[F].blocking(factory.close())
      } yield factory
    )(f => Sync[F].blocking(f.close()))

  implicit class OrientDbGraphOps(orientGraph: OrientBaseGraph) {
    import scala.jdk.CollectionConverters._

    protected def blockingIteratorQuery(query: GraphQuery[_]): Iterator[OrientElement] = {
      val (q, args) = query.stringify
      orientGraph
        .command(new OCommandSQL(q))
        .execute[OrientDynaElementIterable](args: _*)
        .iterator()
        .asScala
        .map(_.asInstanceOf[OrientElement])
    }

    private def getVertex[F[_]: Sync](query: GraphQuery[_]): Option[OrientVertex] =
      blockingIteratorQuery(query)
        .collect { case r: OrientVertex @unchecked => r }
        .nextOption()

    def getNode[F[_]: Sync, T: NodeSchema](query: GraphQuery[T]): F[Option[T]] =
      Sync[F].blocking {
        blockingIteratorQuery(query)
          .collect { case r: OrientVertex @unchecked => NodeSchema[T].decode(r.getProperties.asScala.toMap) }
          .nextOption()
      }

    def insertNode[F[_]: Sync, T: NodeSchema](t: T): F[Unit] = Sync[F].blocking {
      val schema = implicitly[NodeSchema[T]]
      val v = orientGraph.addVertex(s"class:${schema.name}")
      schema.encode(t).foreach { case (name, value) =>
        v.setProperty(name, value)
      }
      v.save()
    }

    def removeNodes[F[_]: Sync](query: GraphQuery[_]): F[Unit] = Sync[F].blocking {
      blockingIteratorQuery(query)
        .foreach { t =>
          t.remove()
          t.save()
        }
    }

    def initializeSchema[F[_]: Sync](nodeSchema: NodeSchema[_]): F[Unit] =
      Sync[F].blocking {
        if (Option(orientGraph.getVertexType(nodeSchema.name)).isEmpty) {
          val vertexType = orientGraph.createVertexType(nodeSchema.name)
          nodeSchema.properties.foreach(property => vertexType.createProperty(property.name, property.propertyType))
          nodeSchema.indices.foreach(index => vertexType.createIndex(index.name, index.indexType, index.propertyName))
        }
      }

    def initializeSchema[F[_]: Sync](edgeSchema: EdgeSchema[_, _, _]): F[Unit] =
      Sync[F].blocking {
        if (Option(orientGraph.getEdgeType(edgeSchema.name)).isEmpty) {
          val edgeType = orientGraph.createEdgeType(edgeSchema.name)
          edgeSchema.properties.foreach(property => edgeType.createProperty(property.name, property.propertyType))
          edgeSchema.indices.foreach(index => edgeType.createIndex(index.name, index.indexType, index.propertyName))
        }
      }

    def insertNodeBuilder[F[_]: Sync, T: NodeSchema](t: T): InsertNodeBuilder[F, T] =
      InsertNodeBuilder(t, Chain.empty, Chain.empty)

    case class InsertNodeBuilder[F[_]: Sync, T: NodeSchema] private (
      t:        T,
      outEdges: Chain[(String, Map[String, Any], GraphQuery[_])],
      inEdges:  Chain[(String, Map[String, Any], GraphQuery[_])]
    ) {

      def withEdgeTo[O, E: EdgeSchema[*, T, O]](edge: E, o: GraphQuery[O]): InsertNodeBuilder[F, T] =
        copy(outEdges =
          outEdges.append(
            (
              implicitly[EdgeSchema[E, T, O]].name,
              implicitly[EdgeSchema[E, T, O]].encode(edge),
              o
            )
          )
        )

      def withEdgeFrom[O, E: EdgeSchema[*, O, T]](edge: E, o: GraphQuery[O]): InsertNodeBuilder[F, T] =
        copy(inEdges =
          inEdges.append(
            (
              implicitly[EdgeSchema[E, O, T]].name,
              implicitly[EdgeSchema[E, O, T]].encode(edge),
              o
            )
          )
        )

      def run(): F[Unit] = Sync[F].blocking {
        val schema = implicitly[NodeSchema[T]]
        val v = orientGraph.addVertex(s"class:${schema.name}")
        schema.encode(t).foreach { case (name, value) =>
          v.setProperty(name, value)
        }
        v.save()
        outEdges.iterator.foreach { e =>
          val edge = orientGraph.addEdge(
            s"class:${e._1}",
            v,
            getVertex(e._3).getOrElse {
              val (q, args) = e._3.stringify
              throw new NoSuchElementException(s"`$q` Args: ${args.mkString(",")}")
            },
            null
          )
          e._2.foreach { case (key, value) => edge.setProperty(key, value) }
        }
        inEdges.iterator.foreach { e =>
          val edge = orientGraph.addEdge(
            s"class:${e._1}",
            getVertex(e._3).getOrElse {
              val (q, args) = e._3.stringify
              throw new NoSuchElementException(s"`$q` Args: ${args.mkString(",")}")
            },
            v,
            null
          )
          e._2.foreach { case (key, value) => edge.setProperty(key, value) }
        }
      }

    }

    def insertEdge[F[_]: Sync, T: EdgeSchema[*, Src, Dest], Src, Dest](
      t:         T,
      srcQuery:  GraphQuery[Src],
      destQuery: GraphQuery[Dest]
    ): F[Unit] = Sync[F].blocking {
      val schema = implicitly[EdgeSchema[T, Src, Dest]]
      val (_srcQuery, srcArgs) = srcQuery.stringify
      val (_destQuery, destArgs) = destQuery.stringify
      // TODO: Encode edge properties
      val query =
        s"""CREATE EDGE ${schema.name}
           |  FROM (${_srcQuery} LIMIT 1)
           |  TO (${_destQuery} LIMIT 1)
           |""".stripMargin
      val args = srcArgs ++ destArgs
      orientGraph.command(new OCommandSQL(query)).execute[Unit](args: _*)
    }
  }

}
