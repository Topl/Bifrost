package co.topl.consensus.genesis

import co.topl.consensus.Forger.ChainParams
import co.topl.consensus.ProtocolVersioner
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.block.PersistentNodeViewModifier.PNVMVersion
import co.topl.modifier.box.PolyBox
import co.topl.settings.GenesisFromBlockJsonSettings
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.Base58Data
import co.topl.utils.{Int128, NetworkType}
import io.circe.parser

import scala.collection.immutable.ListMap
import scala.util.Try

case class GenesisFromBlockJson(
  settings:     GenesisFromBlockJsonSettings,
  networkType:  NetworkType,
  protocolMngr: ProtocolVersioner
) extends GenesisProvider {

  implicit override val networkPrefix: NetworkPrefix = networkType.netPrefix

  override protected val blockChecksum: ModifierId = ModifierId.fromBase58(Base58Data.unsafe(settings.blockChecksum))

  override protected val blockVersion: PNVMVersion = protocolMngr.blockVersion(1)

  val block: Block = {
    val blockFromJson = readJson(settings.providedJsonGenesisPath)(networkType.netPrefix)
    require(
      blockFromJson.id == blockChecksum,
      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${blockFromJson.id} does not match the required block for the chosen network mode.${Console.RESET}"
    )
    blockFromJson
  }

  override protected val initialDifficulty: Long = block.difficulty

  override protected[genesis] val members: ListMap[String, Int128] = ListMap.empty

  override def getGenesisBlock: Try[(Block, ChainParams)] = Try(formNewBlock)

  def formNewBlock: (Block, ChainParams) = {
    val privateTotalStake = block.transactions
      .flatMap(_.newBoxes.map { box =>
        val stake: Int128 = box match {
          case box: PolyBox => box.value.quantity
          case _            => 0
        }
        box.id.toString -> stake
      })
      .toMap
      .values
      .sum

    log.debug(s"Initialize state with block $block")

    (block, ChainParams(privateTotalStake, block.difficulty))
  }

  protected def readJson(filename: String)(implicit networkPrefix: NetworkPrefix): Block = {
    // read data from disk
    val src = scala.io.Source.fromFile(filename)

    // attempt to retrieve the required keyfile type from the data that was just read
    val block: Block = parser.parse(src.mkString) match {
      case Left(ex) => throw ex
      case Right(json) =>
        json.as[Block].getOrThrow(ex => new Exception(s"Could not parse blcok Json: $ex"))
    }

    // close the stream and return the keyfile
    src.close()
    block
  }
}
