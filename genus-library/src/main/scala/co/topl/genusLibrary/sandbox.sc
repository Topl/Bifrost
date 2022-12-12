import com.orientechnologies.orient.graph.batch.OGraphBatchInsert
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import com.tinkerpop.blueprints.util.wrappers.batch.{BatchGraph, VertexIDType}

import scala.collection.convert.AsScalaConverters
import scala.collection.mutable
import scala.jdk.javaapi.CollectionConverters.{asJava, asScala}

val graph: OrientGraph = new OrientGraph("memory:test", "admin", "admin")

val personVertexType = graph.createVertexType("Person")
val addressVertexType = graph.createVertexType("Address")

val vPersonJohn = graph.addVertex("class:Person")
vPersonJohn.setProperty("firstName", "John")
vPersonJohn.setProperty("lastName", "Smith")

val vPersonIgnacio = graph.addVertex("class:Person")
vPersonIgnacio.setProperty("firstName", "Ignacio")
vPersonIgnacio.setProperty("lastName", "Soto")

val vAddress = graph.addVertex("class:Address")
vAddress.setProperty("street", "Van Ness Ave.")
vAddress.setProperty("city", "San Francisco")
vAddress.setProperty("state", "California")

val eLives = graph.addEdge(null, vPersonJohn, vAddress, "lives")
val eLives = graph.addEdge(null, vPersonIgnacio, vAddress, "lives")

val personVertexTypeClass = personVertexType.getName

val vertices = graph.getVerticesOfClass(personVertexTypeClass)

vertices.forEach { v =>
  val propertyKeys = asScala(v.getPropertyKeys)
  propertyKeys.foreach(key => println(s"key: $key value: ${v.getProperty(key)}"))
}

val johnVertex = asScala(graph.getVertices(personVertexTypeClass, List("firstName").toArray, List("John").toArray)).head

val batchGraph = new BatchGraph[OrientGraph](graph, VertexIDType.STRING, 1)

//val modifiableJohnVertex = batchGraph.getVertex(johnVertex.getId)

//johnVertex.setProperty("firstName", "Jonathan")

batchGraph.toString

batchGraph.commit()

val updatedVertices = graph.getVerticesOfClass(personVertexTypeClass)

updatedVertices.forEach { v =>
  val propertyKeys = asScala(v.getPropertyKeys)
  propertyKeys.foreach(key => println(s"key: $key value: ${v.getProperty(key)}"))
}