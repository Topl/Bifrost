//package co.topl.consensus.genesis
//
//import co.topl.codecs._
//import co.topl.consensus.Forger.ChainParams
//import co.topl.modifier.ModifierId
//import co.topl.modifier.block.Block
//import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
//import co.topl.modifier.box.ArbitBox
//import co.topl.settings.GenesisFromBlockJsonSettings
//import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
//import co.topl.utils.NetworkType.NetworkPrefix
//import co.topl.utils.StringDataTypes.Base58Data
//import co.topl.utils.{Int128, NetworkType}
//import io.circe.parser
//
//import scala.collection.immutable.ListMap
//import scala.util.Try
//
//case class GenesisFromBlockJson(
//  settings:    GenesisFromBlockJsonSettings,
//  networkType: NetworkType
//) extends GenesisLegacyProvider {
//
//  implicit override val networkPrefix: NetworkPrefix = networkType.netPrefix
//
//  // should checksum be a byte category?
//  override protected val blockChecksum: ModifierId =
//    Base58Data.unsafe(settings.blockChecksum).encodeAsBytes.decodeTransmitted[ModifierId].getOrThrow()
//
//  val block: Block = {
//    val blockFromJson = readJson(settings.providedJsonGenesisPath)(networkType.netPrefix)
//    require(
//      blockFromJson.id == blockChecksum,
//      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
//      s"with id ${blockFromJson.id} does not match the required block for the chosen network mode.${Console.RESET}"
//    )
//    blockFromJson
//  }
//
//  override protected val blockVersion: PNVMVersion = block.version
//
//  override protected val initialDifficulty: Long = block.difficulty
//
//  override protected[genesis] val members: ListMap[String, Int128] = ListMap.empty
//
//  override def getGenesisBlock: Try[(Block, ChainParams)] = Try(formNewBlock)
//
//  def formNewBlock: (Block, ChainParams) = {
//    val totalStake = calcTotalStake(block)
//
//    log.debug(s"Initialize state with block $block")
//
//    (block, ChainParams(totalStake, block.difficulty))
//  }
//
//  protected def readJson(filename: String)(implicit networkPrefix: NetworkPrefix): Block = {
//    // read data from disk
//    val src = scala.io.Source.fromFile(filename)
//
//    // attempt to retrieve the required keyfile type from the data that was just read
//    val block: Block = parser.parse(src.mkString) match {
//      case Left(ex) => throw ex
//      case Right(json) =>
//        json.as[Block].getOrThrow(ex => new Exception(s"Could not parse blcok Json: $ex"))
//    }
//
//    // close the stream and return the keyfile
//    src.close()
//    block
//  }
//}
