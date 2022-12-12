package co.topl.genusLibrary.orientDb.wrapper

import com.tinkerpop.blueprints.impls.orient.OrientGraph
import com.tinkerpop.blueprints.util.wrappers.batch.BatchGraph

class OrientBatchGraph(graph: OrientGraph) extends BatchGraph[OrientGraph](graph)