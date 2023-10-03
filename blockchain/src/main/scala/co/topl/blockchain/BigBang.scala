package co.topl.blockchain

import cats.Parallel
import cats.data.{EitherT, ReaderT}
import cats.effect.Sync
import cats.implicits._
import co.topl.brambl.models._
import co.topl.brambl.models.box.Value
import co.topl.brambl.models.transaction._
import co.topl.brambl.syntax._
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.Transmittable
import co.topl.config.ApplicationConfig
import co.topl.consensus.algebras.BlockHeaderToBodyValidationAlgebra
import co.topl.consensus.models._
import co.topl.crypto.hash.Blake2b256
import co.topl.models._
import co.topl.models.utility.HasLength.instances.byteStringLength
import co.topl.models.utility._
import co.topl.node.models._
import co.topl.numerics.implicits._
import co.topl.typeclasses.implicits._
import com.google.common.primitives.Longs
import com.google.protobuf.ByteString

/**
 * The beginning of everything.  ("everything" of course just means the first block of a blockchain)
 */
object BigBang {

  /**
   * Represents a way of configuring the variables of the Big Bang Block.
   * @param timestamp The unix epoch (in milliseconds) of the block.  Also represents the timestamp of the first slot
   *                  of the blockchain's clock.
   * @param transactions The initial set of transactions with outputs for the blockchain.  This generally includes the
   *                     initial distribution to the blockchain's initial investors.  In addition, it should specify
   *                     the staking registrations for the blockchain's initial operators.
   * @param etaPrefix a sequence of bytes to be prepended to the value that gets hashed to produce the Big Bang Eta.
   */
  case class Config(
    timestamp:       Timestamp,
    transactions:    List[IoTransaction],
    etaPrefix:       Bytes = Config.DefaultEtaPrefix,
    protocolVersion: ProtocolVersion
  )

  object Config {

    val DefaultEtaPrefix: Bytes = ByteString.copyFromUtf8("genesis")

  }

  /**
   * Constructs a full block using the given Big Bang Configuration
   */
  def fromConfig(config: Config): FullBlock = {

    val eta: Eta =
      Sized.strictUnsafe(
        new Blake2b256().hash(
          (config.etaPrefix.toByteArray +:
          Longs.toByteArray(config.timestamp) +:
          config.transactions.map(_.id.value.toByteArray)): _*
        )
      )

    val header =
      BlockHeader(
        parentHeaderId = ParentId,
        parentSlot = ParentSlot,
        txRoot = config.transactions.merkleTreeRootHash.data,
        bloomFilter = config.transactions.bloomFilter.data,
        timestamp = config.timestamp,
        height = Height,
        slot = Slot,
        eligibilityCertificate = vrfCertificate(eta),
        operationalCertificate = kesCertificate,
        metadata = ByteString.EMPTY,
        address = StakingAddress(zeroBytes(Lengths.`32`).data),
        version = config.protocolVersion
      ).embedId
    FullBlock(header, FullBlockBody(config.transactions))
  }

  val ParentId: BlockId = BlockId(ByteString.copyFrom(Array.fill[Byte](32)(0)))
  val ParentSlot: Slot = -1
  val Slot = 0
  val Height = 1

  def vrfCertificate(eta: Eta): EligibilityCertificate = EligibilityCertificate(
    ByteString.copyFrom(Array.fill[Byte](80)(0)),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    eta = eta.data
  )

  val kesCertificate: OperationalCertificate = OperationalCertificate(
    VerificationKeyKesProduct(ByteString.copyFrom(Array.fill[Byte](32)(0)), 0),
    SignatureKesProduct(
      SignatureKesSum(
        ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ByteString.copyFrom(Array.fill[Byte](64)(0)),
        Vector.empty
      ),
      SignatureKesSum(
        ByteString.copyFrom(Array.fill[Byte](32)(0)),
        ByteString.copyFrom(Array.fill[Byte](64)(0)),
        Vector.empty
      ),
      ByteString.copyFrom(Array.fill[Byte](32)(0))
    ),
    ByteString.copyFrom(Array.fill[Byte](32)(0)),
    ByteString.copyFrom(Array.fill[Byte](64)(0))
  )

  def zeroBytes[L <: Length](implicit l: L): Sized.Strict[Bytes, L] =
    Sized.strictUnsafe[Bytes, L](ByteString.copyFrom(Array.fill(l.value)(0: Byte)))

  /**
   * Loads the given FullBlock by its ID using the supplied file reader function.
   * The header will be retrieved first, using the file name ${block ID}.header.pbuf.
   * Next, the body will be retrieved using the file name ${block ID}.body.pbuf.
   * Next, all transactions will be retrieved using the file names ${transaction ID}.transaction.pbuf.
   * @param readFile A function which retrieves a file by name
   * @param blockId The block ID to retrieve
   * @return a FullBlock associated with the given block ID
   */
  def fromRemote[F[_]: Sync: Parallel](
    readFile: ReaderT[F, String, Array[Byte]]
  )(txRootValidation: BlockHeaderToBodyValidationAlgebra[F])(blockId: BlockId): F[FullBlock] =
    (
      for {
        genesisBlockIdStr <- EitherT.liftF(Sync[F].delay(blockId.show))
        header <-
          EitherT(
            readFile(s"$genesisBlockIdStr.header.pbuf")
              .map(ByteString.copyFrom)
              .map(Transmittable[BlockHeader].fromTransmittableBytes)
          )
            .map(_.embedId)
            .ensure("Computed header ID is not the same as requested header ID")(_.id == blockId)
        body <-
          EitherT(
            readFile(s"$genesisBlockIdStr.body.pbuf")
              .map(ByteString.copyFrom)
              .map(Transmittable[BlockBody].fromTransmittableBytes)
          )
        _ <- EitherT(txRootValidation.validate(Block(header, body)))
          .leftMap(_.toString)
        fetchTransaction = (id: TransactionId) =>
          EitherT(
            readFile(s"${id.show}.transaction.pbuf")
              .map(ByteString.copyFrom)
              .map(Transmittable[IoTransaction].fromTransmittableBytes)
          )
            .map(_.embedId)
            .ensure("Computed transaction ID is not the same as requested transaction ID")(_.id == id)
        transactions      <- body.transactionIds.parTraverse(fetchTransaction)
        rewardTransaction <- body.rewardTransactionId.parTraverse(fetchTransaction)
        fullBlockBody = FullBlockBody(transactions, rewardTransaction)
        fullBlock = FullBlock(header, fullBlockBody)
      } yield fullBlock
    ).leftMap(new IllegalArgumentException(_)).rethrowT

  def extractProtocol(block: FullBlock): Either[String, ApplicationConfig.Bifrost.Protocol] =
    block.fullBody.transactions.flatMap(_.outputs).map(_.value.value).flatMap(_.updateProposal).toList match {
      case List(proposal) => updateProposalToProtocol(proposal)
      case Nil            => Left("Protocol not defined")
      case _              => Left("Multiple protocols defined")
    }

  def updateProposalToProtocol(proposal: Value.UpdateProposal): Either[String, ApplicationConfig.Bifrost.Protocol] =
    for {
      fEffective              <- proposal.fEffective.toRight("Missing fEffective")
      vrfLddCutoff            <- proposal.vrfLddCutoff.toRight("Missing vrfLddCutoff")
      vrfPrecision            <- proposal.vrfPrecision.toRight("Missing vrfPrecision")
      vrfBaselineDifficulty   <- proposal.vrfBaselineDifficulty.toRight("Missing vrfBaselineDifficulty")
      vrfAmplitude            <- proposal.vrfAmplitude.toRight("Missing vrfAmplitude")
      chainSelectionKLookback <- proposal.chainSelectionKLookback.toRight("Missing chainSelectionKLookback")
      slotDuration            <- proposal.slotDuration.toRight("Missing slotDuration")
      forwardBiasedSlotWindow <- proposal.forwardBiasedSlotWindow.toRight("Missing forwardBiasedSlotWindow")
      operationalPeriodsPerEpoch <- proposal.operationalPeriodsPerEpoch.toRight(
        "Missing operationalPeriodsPerEpoch"
      )
      kesKeyHours   <- proposal.kesKeyHours.toRight("Missing kesKeyHours")
      kesKeyMinutes <- proposal.kesKeyMinutes.toRight("Missing kesKeyMinutes")
    } yield ApplicationConfig.Bifrost.Protocol(
      "2.0.0",
      fEffective,
      vrfLddCutoff,
      vrfPrecision,
      vrfBaselineDifficulty,
      vrfAmplitude,
      chainSelectionKLookback,
      slotDuration,
      forwardBiasedSlotWindow,
      operationalPeriodsPerEpoch,
      kesKeyHours,
      kesKeyMinutes
    )

  def protocolToUpdateProposal(protocol: ApplicationConfig.Bifrost.Protocol): Value =
    Value.defaultInstance.withUpdateProposal(
      Value.UpdateProposal(
        label = "genesis",
        fEffective = (protocol.fEffective: co.topl.node.models.Ratio).some,
        vrfLddCutoff = protocol.vrfLddCutoff.some,
        vrfPrecision = protocol.vrfPrecision.some,
        vrfBaselineDifficulty = (protocol.vrfBaselineDifficulty: co.topl.node.models.Ratio).some,
        vrfAmplitude = (protocol.vrfAmplitude: co.topl.node.models.Ratio).some,
        chainSelectionKLookback = protocol.chainSelectionKLookback.some,
        slotDuration = (protocol.slotDuration: com.google.protobuf.duration.Duration).some,
        forwardBiasedSlotWindow = protocol.forwardBiasedSlotWindow.some,
        operationalPeriodsPerEpoch = protocol.operationalPeriodsPerEpoch.some,
        kesKeyHours = protocol.kesKeyHours.some,
        kesKeyMinutes = protocol.kesKeyMinutes.some
      )
    )
}
