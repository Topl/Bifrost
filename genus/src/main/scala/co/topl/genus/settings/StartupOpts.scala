package co.topl.genus.settings

import mainargs._

@main
final case class StartupOpts(
  @arg(short = 'm', name = "mongo")
  mongoConnectionString: String,
  @arg(short = 'd', name = "database")
  mongoDatabaseName: String = "chain_data",
  @arg(short = 't', name = "tx-collection")
  transactionsCollectionName: String = "confirmed_txes",
  @arg(short = 'b', name = "block-collection")
  blocksCollectionName: String = "blocks",
  @arg(short = 'i')
  ip: String = "0.0.0.0",
  @arg(short = 'p')
  port: Int = 3031,
  @arg
  localDatabaseName: String = "local",
  @arg
  oplogCollectionName: String = "oplog.rs",
  @arg(short = 't', name = "query-timeout")
  queryTimeout: Int = 5000
)
