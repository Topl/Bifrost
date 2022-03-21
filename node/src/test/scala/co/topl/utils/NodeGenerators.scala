package co.topl.utils

import co.topl.attestation.Address
import co.topl.consensus.KeyManager.AddressGenerationSettings
import co.topl.consensus.{GenesisProvider, NxtConsensus, ProtocolVersioner}
import co.topl.modifier.block.Block
import co.topl.settings.{ProtocolConfigurations, ProtocolSettings, Version}
import org.scalacheck.Gen
import org.scalatest.Suite

trait NodeGenerators extends CommonGenerators {
  self: Suite =>

  lazy val versionGen: Gen[Version] = for {
    first  <- Gen.choose(0: Byte, Byte.MaxValue)
    second <- Gen.choose(0: Byte, Byte.MaxValue)
    third  <- Gen.choose(0: Byte, Byte.MaxValue)
  } yield new Version(first, second, third)

  lazy val genesisGenerationSettingsGen: Gen[GenesisProvider.Strategies.Generation] = for {
    version           <- versionGen
    balance           <- positiveLongGen
    initialDifficulty <- positiveLongGen
  } yield GenesisProvider.Strategies.Generation(version, balance, initialDifficulty)

  lazy val addressGenerationSettingsGen: Gen[AddressGenerationSettings] = for {
    numberOfParticipants <- positiveMediumIntGen
    genesisSeed          <- stringGen
  } yield AddressGenerationSettings(numberOfParticipants, Some(genesisSeed))

  lazy val genesisBlockGen: Gen[Block] = nxtConsensusGenesisGen.sample.get.block

  lazy val nxtConsensusGenesisGen: Gen[NxtConsensus.Genesis] = for {
    addresses         <- nonEmptySetAddressGen
    balancePerAddress <- positiveLongGen
    initialDifficulty <- positiveLongGen
    blockVersion      <- Gen.choose[Byte](0, Byte.MaxValue)
  } yield GenesisProvider.construct(addresses, balancePerAddress, initialDifficulty, blockVersion)

  lazy val protocolVersionerGen: Gen[ProtocolVersioner] = for {
    appVersion       <- versionGen
    protocolSettings <- Gen.nonEmptyListOf(protocolSettingsGen)
  } yield ProtocolVersioner(appVersion, protocolSettings)

  lazy val protocolSettingsGen: Gen[ProtocolSettings] = for {
    minAppVersion <- versionGen
    startBlock    <- positiveLongGen
    blockVersion  <- Gen.choose[Byte](0, Byte.MaxValue)
    value         <- dionProtocolConfigGen
  } yield ProtocolSettings(minAppVersion, startBlock, blockVersion, value)

  lazy val dionProtocolConfigGen: Gen[ProtocolConfigurations.Dion] = for {
    targetBlockTime <- Gen.finiteDuration
    numTxPerBlock   <- positiveMediumIntGen
    inflationRate   <- positiveMediumIntGen
    lookBackDepth   <- positiveMediumIntGen
  } yield ProtocolConfigurations.Dion(targetBlockTime, numTxPerBlock, inflationRate, lookBackDepth)

}
