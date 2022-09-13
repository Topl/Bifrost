package co.topl.genus.settings

/**
 * Application settings for the Genus server application.
 * @param mongoConnectionString the connection string for the MongoDB database
 * @param mongoDatabaseName the name of the MongoDB database
 * @param transactionsCollectionName the name of the MongoDB collection for storing transactions
 * @param blocksCollectionName the name of the MongoDB collection for storing blocks
 * @param ip the IP address of the server
 * @param port the port of the server
 * @param localDatabaseName the name of the MongoDB local database
 * @param oplogCollectionName the name of the MongoDB oplog collection
 * @param queryTimeout the timeout length for queries in milliseconds
 */
final case class ApplicationSettings(
  mongoConnectionString:      String,
  mongoDatabaseName:          String,
  transactionsCollectionName: String,
  blocksCollectionName:       String,
  ip:                         String,
  port:                       Int,
  webPort:                    Int,
  disableAuth:                Boolean,
  apiKeyHash:                 String,
  queryTimeout:               Int,
  subBatchSize:               Int,
  subBatchSleepDuration:      Int
)

object ApplicationSettings {

  /**
   * Merges the application settings with the startup options provided as app arguments.
   * @param settings the application settings
   * @param options the startup options provided to the app
   * @return the merged application settings
   */
  def mergeWithStartup(settings: ApplicationSettings, options: StartupOptions): ApplicationSettings =
    ApplicationSettings(
      options.mongoConnectionString.getOrElse(settings.mongoConnectionString),
      options.mongoDatabaseName.getOrElse(settings.mongoDatabaseName),
      options.transactionsCollectionName.getOrElse(settings.transactionsCollectionName),
      options.blocksCollectionName.getOrElse(settings.blocksCollectionName),
      options.ip.getOrElse(settings.ip),
      options.port.getOrElse(settings.port),
      options.webPort.getOrElse(settings.webPort),
      options.disableAuth.getOrElse(settings.disableAuth),
      options.apiKeyHash.getOrElse(settings.apiKeyHash),
      options.queryTimeout.getOrElse(settings.queryTimeout),
      options.subBatchSize.getOrElse(settings.subBatchSize),
      options.subBatchSleepDuration.getOrElse(settings.subBatchSleepDuration)
    )
}
