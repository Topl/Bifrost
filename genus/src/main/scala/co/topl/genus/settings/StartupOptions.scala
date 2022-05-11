package co.topl.genus.settings

import mainargs._

/**
 * Startup options that can be passed to the application as arguments.
 * @param mongoConnectionString the connection string to the MongoDB database
 * @param mongoDatabaseName the name of the MongoDB database
 * @param transactionsCollectionName the name of the MongoDB collection that stores the transactions
 * @param blocksCollectionName the name of the MongoDB collection that stores the blocks
 * @param ip the IP address for the server
 * @param port the port for the server
 * @param localDatabaseName the name of the MongoDB local database
 * @param oplogCollectionName the name of the MongoDB oplog collection
 * @param queryTimeout the query timeout in milliseconds
 * @param configurationPath the path to the configuration file
 */
@main
final case class StartupOptions(
  @arg(short = 'm', name = "mongo")
  mongoConnectionString: Option[String] = None,
  @arg(short = 'd', name = "database")
  mongoDatabaseName: Option[String] = None,
  @arg(short = 't', name = "tx-collection")
  transactionsCollectionName: Option[String] = None,
  @arg(short = 'b', name = "block-collection")
  blocksCollectionName: Option[String] = None,
  @arg(short = 'i')
  ip: Option[String] = None,
  @arg(short = 'p')
  port: Option[Int] = None,
  @arg
  disableAuth: Option[Boolean] = None,
  @arg
  apiKeyHash: Option[String] = None,
  @arg(short = 't', name = "query-timeout")
  queryTimeout: Option[Int] = None,
  @arg(short = 'c', name = "config")
  configurationPath: Option[String] = None,
  @arg
  subBatchSize: Option[Int] = None,
  @arg
  subBatchSleepDuration: Option[Int] = None
)
