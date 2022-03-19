package co.topl.consensus.genesis

import akka.actor.typed.ActorSystem
import cats.implicits._
import co.topl.attestation.keyManagement.PrivateKeyCurve25519
import co.topl.attestation.{Address, EvidenceProducer, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.codecs._
import co.topl.consensus.KeyManager.StartupKeyView
import co.topl.consensus.NxtConsensus
import co.topl.consensus.genesis.GenesisProvider.StrategySettings.{FromBlockJsonSettings, GenerationSettings}
import co.topl.crypto.{PrivateKey, PublicKey}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, Box, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.{AppSettings, GenesisStrategies, Version}
import co.topl.utils.Int128
import co.topl.utils.NetworkType.NetworkPrefix
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import io.circe.parser

import scala.collection.immutable.ListMap
import scala.concurrent.ExecutionContext

class GenesisProvider(genesisBlockVersion: Byte, startupKeyView: StartupKeyView) {

  def fetchGenesis(
    settings: AppSettings
  )(implicit
    system:        ActorSystem[_],
    ec:            ExecutionContext,
    networkPrefix: NetworkPrefix
  ): Either[GenesisProvider.Failure, NxtConsensus.Genesis] = settings.application.genesis.strategy match {
    case GenesisStrategies.FromBlockJson =>
      for {
        strategy <- Either.fromOption(
          settings.application.genesis.fromBlockJson,
          GenesisProvider.Failures.GenesisBlockJsonSettingsNotFound: GenesisProvider.Failure
        )
        genesis <- fromJsonGenesisProvider(strategy)
      } yield genesis

    case GenesisStrategies.Generated =>
      settings.application.genesis.generated
        .map(fromGeneratedProvider)
        .toRight(GenesisProvider.Failures.GenesisBlockJsonSettingsNotFound)
  }

  private def fromJsonGenesisProvider(strategy: FromBlockJsonSettings)(implicit
    networkPrefix:                                       NetworkPrefix
  ): Either[GenesisProvider.Failure, NxtConsensus.Genesis] = for {
    src   <- scala.io.Source.fromFile(strategy.providedJsonGenesisPath).asRight[GenesisProvider.Failure]
    json  <- parser.parse(src.mkString).leftMap(GenesisProvider.Failures.FailedToReadBlockJson)
    block <- json.as[Block].leftMap(e => GenesisProvider.Failures.FailedToeDecodeBlockJson(e.message))
    expectedBlockId <- Base58Data
      .unsafe(strategy.blockChecksum)
      .encodeAsBytes
      .decodeTransmitted[ModifierId]
      .leftMap(_ => GenesisProvider.Failures.InvalidBlockChecksum)
    validBlock <- Either.cond(
      block.id == expectedBlockId,
      block,
      GenesisProvider.Failures.BlockChecksumMismatch(
        new IllegalArgumentException(
          s"Expected block with id $expectedBlockId, but found block with id ${block.id}"
        )
      )
    )
    stakeAmounts = validBlock.transactions.flatMap(_.newBoxes.map {
      case box: ArbitBox => box.value.quantity
      case _             => Int128(0)
    })
  } yield NxtConsensus.Genesis(validBlock, NxtConsensus.State(stakeAmounts.sum, validBlock.difficulty, 0L, 0L))

  private def fromGeneratedProvider(
    strategy:               GenerationSettings
  )(implicit networkPrefix: NetworkPrefix): NxtConsensus.Genesis =
    GenesisProvider.construct(
      startupKeyView.addresses,
      strategy.balanceForEachParticipant,
      strategy.initialDifficulty,
      genesisBlockVersion
    )
}

object GenesisProvider {

  final case class GenesisTransactionParams(
    from:       IndexedSeq[(Address, Box.Nonce)],
    to:         IndexedSeq[(Address, SimpleValue)],
    signatures: ListMap[PublicKeyPropositionCurve25519, SignatureCurve25519],
    fee:        Int128,
    timestamp:  Long,
    data:       Option[Latin1Data],
    minting:    Boolean
  )

  private[genesis] def construct(
    addresses:                 Set[Address],
    balanceForEachParticipant: Long,
    initialDifficulty:         Long,
    blockVersion:              Byte
  )(implicit
    networkPrefix: NetworkPrefix
  ): NxtConsensus.Genesis = {

    val genesisAcctCurve25519 =
      new PrivateKeyCurve25519(PrivateKey(Array.fill(32)(2: Byte)), PublicKey(Array.fill(32)(2: Byte)))

    val totalStake = addresses.size * balanceForEachParticipant

    val txInput: GenesisTransactionParams = GenesisTransactionParams(
      IndexedSeq(),
      (genesisAcctCurve25519.publicImage.address -> SimpleValue(0L)) +: addresses
        .map(_ -> SimpleValue(balanceForEachParticipant))
        .toIndexedSeq,
      ListMap(genesisAcctCurve25519.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      minting = true
    )

    val block = Block(
      ModifierId.genesisParentId,
      0L,
      ArbitBox(
        EvidenceProducer[PublicKeyPropositionCurve25519].generateEvidence(genesisAcctCurve25519.publicImage),
        0,
        SimpleValue(totalStake)
      ),
      genesisAcctCurve25519.publicImage,
      SignatureCurve25519.genesis,
      1L,
      initialDifficulty,
      Seq(
        ArbitTransfer[PublicKeyPropositionCurve25519](
          txInput.from,
          (txInput.to.head._1, SimpleValue(0)) +: txInput.to, // first 'to'' is feeChangeOutput
          txInput.signatures,
          txInput.fee,
          txInput.timestamp,
          txInput.data,
          txInput.minting
        ),
        PolyTransfer[PublicKeyPropositionCurve25519](
          txInput.from,
          txInput.to,
          txInput.signatures,
          txInput.fee,
          txInput.timestamp,
          txInput.data,
          txInput.minting
        )
      ),
      blockVersion
    )

    val state = NxtConsensus.State(Int128(totalStake), initialDifficulty, 0L, 0L)

    NxtConsensus.Genesis(block, state)
  }

  sealed abstract class Failure

  object Failures {
    case object GenesisGeneratedSettingsNotFound extends Failure
    case object GenesisBlockJsonSettingsNotFound extends Failure
    case class FailedToReadBlockJson(reason: Throwable) extends Failure
    case class FailedToeDecodeBlockJson(message: String) extends Failure
    case class BlockChecksumMismatch(reason: Throwable) extends Failure
    case object InvalidBlockChecksum extends Failure
  }

  sealed abstract class StrategySetting

  object StrategySettings {

    case class GenerationSettings(
      genesisApplicationVersion: Version,
      balanceForEachParticipant: Long,
      initialDifficulty:         Long
    ) extends StrategySetting

    case class FromBlockJsonSettings(providedJsonGenesisPath: String, blockChecksum: String) extends StrategySetting
  }

}
