package co.topl.genusLibrary.orientDb.wrapper

import co.topl.genusLibrary.orientDb.{OrientProperties, VertexSchema}
import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import com.tinkerpop.blueprints.util.wrappers.batch.{BatchGraph, VertexIDType}
import co.topl.models.ModelGenerators._
import munit.ScalaCheckSuite
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalamock.munit.AsyncMockFactory

import scala.util.Random

class OpenForCommitsBatchGraphSpec extends ScalaCheckSuite with AsyncMockFactory {

  def orientDbPropertiesGen: Gen[OrientProperties] = for {
    infiniteOrientLongProperties <- Gen.infiniteStream(
      Gen.zip(
        Gen.asciiPrintableStr,
        Gen.long
      )
    )
    orientLongProperties = infiniteOrientLongProperties.take(Random.nextInt(100)).toMap
    orientProperties = orientLongProperties.asInstanceOf[Map[String, AnyRef]]
  } yield orientProperties

  implicit val arbitraryOrientDbProperties: Arbitrary[OrientProperties] = Arbitrary(orientDbPropertiesGen)

  test("On vertex creation from an element, the vertex and the element should be returned") {
    Prop.forAll {
      (
        elem:       String,
        schemaName: String,
        encodedElem: OrientProperties
      ) =>
        withMock {

          val orientBatchGraph: OrientBatchGraph = mock[OrientBatchGraph]
          implicit val stringVertexSchema: VertexSchema[String] = mock[VertexSchema[String]]
          val vertex: Vertex = mock[Vertex]

          (() => stringVertexSchema.name).expects().returning(schemaName).once()
          (orientBatchGraph.addVertex(_: AnyRef)).expects(s"class:$schemaName").returning(vertex).once()
          (stringVertexSchema.encode _).expects(elem).returning(encodedElem).once()
          encodedElem.foreach {
            case (key, value) => (vertex.setProperty _).expects(key, value).returning((): Unit).once()
          }

          val batchGraph = new OpenForCommitsBatchGraph(orientBatchGraph)

          val (elemResult, vertexResult) = batchGraph.createVertex(elem)

          assertEquals(elemResult, elem)
          assertEquals(vertexResult, vertex)

        }
    }
  }

}
