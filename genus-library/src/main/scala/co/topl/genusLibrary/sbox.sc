import com.orientechnologies.orient.core.db.document.ODatabaseDocumentTx
import com.orientechnologies.orient.core.db.{ODatabasePool, ODatabaseSession, ODatabaseType, OrientDB, OrientDBConfig, OrientDBConfigBuilder}
import com.orientechnologies.orient.core.record.impl.ODocument
import com.tinkerpop.blueprints.impls.orient.OrientGraph
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactory
import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.tinkerpop.blueprints.util.wrappers.batch.BatchGraph

//val graph: OrientGraph = new OrientGraph("remote:172.17.0.2/orientdb", "root", "rootpwd")

//val rawGraph: ODatabaseDocumentTx = graph.getRawGraph

val db: OrientDB = new OrientDB("remote:172.17.0.2/orientdb", "root", "rootpwd", null)

db.isOpen

db.createIfNotExists("test", ODatabaseType.MEMORY)

val poolCfg = OrientDBConfig.builder
poolCfg.addConfig(OGlobalConfiguration.DB_POOL_MIN, 5)
poolCfg.addConfig(OGlobalConfiguration.DB_POOL_MAX, 10)

val pool = new ODatabasePool(db, "test", "root", "rootpwd", poolCfg.build)

val sessionTx: ODatabaseSession = pool.acquire()

sessionTx.begin()

val animal = new ODocument(sessionTx, "Animal")

animal.field("name", "Gaudi")
animal.field("location", "Madrid")

val session1 = pool.acquire()

session1.countClass("Animal")

animal.save()

val session2 = pool.acquire()

session2.countClass("Animal")

/* Ex if I don't do this. Refer to the method docs for more information
java.lang.IllegalStateException: The current database instance (com.orientechnologies.orient.core.db.ODatabaseDocumentRemotePooled@43b287e7) is not active on the current thread (Thread[NGSession 1: 127.0.0.1: compile,5,main]). Current active database is: com.orientechnologies.orient.core.db.ODatabaseDocumentRemotePooled@15de030b
  at com.orientechnologies.orient.core.db.document.ODatabaseDocumentAbstract.checkIfActive(ODatabaseDocumentAbstract.java:1876)
  at com.orientechnologies.orient.core.db.document.ODatabaseDocumentAbstract.commit(ODatabaseDocumentAbstract.java:1569)
  at com.orientechnologies.orient.core.db.document.ODatabaseDocumentAbstract.commit(ODatabaseDocumentAbstract.java:1563)
  ... 40 elided
 */
sessionTx.activateOnCurrentThread()

sessionTx.commit()

val session3 = pool.acquire()

session3.countClass("Animal")

/*
val session = db.open("test", "root", "rootpwd")

session.begin()

val animal = new ODocument("Animal")


/*
animal.field("name", "Gaudi")
animal.field("location", "Madrid")

session.save(animal)

session.commit()

session.getClass("Animal")