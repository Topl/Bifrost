package co.topl.storage.server

import org.neo4j.dbms.api.{DatabaseManagementService, DatabaseManagementServiceBuilder}

import java.nio.file.Path

class GraphDbServer extends App {
  val dbName: String = ???
  val dbDirectory: Path = ???
  val managementService = new DatabaseManagementServiceBuilder(dbDirectory).build()
  val graphDb = managementService.database(dbName)

  registerShutdownHook(managementService)

  private def registerShutdownHook(
    managementService: DatabaseManagementService
  ): Unit =
    Runtime.getRuntime.addShutdownHook(new Thread() {

      override def run(): Unit =
        managementService.shutdown()
    })
}
