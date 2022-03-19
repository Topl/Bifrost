package co.topl.consensus

import co.topl.attestation.Address
import co.topl.modifier.block.Block
import co.topl.utils.NetworkType.NetworkPrefix
import org.scalacheck.Gen

object TestGenesisGenerator {

  def get(strategy: GenesisProvider.Strategies.Generation)(implicit
    networkPrefix:  NetworkPrefix,
    addressGen:     Gen[Address]
  ): NxtConsensus.Genesis = {
    val addresses: List[Address] = Gen.listOfN(strategy.numberOfParticipants, addressGen).sample.get
    val blockVersion: NetworkPrefix = Gen.choose(0: Byte, Byte.MaxValue).sample.get
    GenesisProvider.generatedGenesisProvider(addresses.toSet, blockVersion).executeStrategy(strategy)
  }

  def get2(addresses: Set[Address], settings: GenesisGenerationSettings)(implicit
    networkPrefix:    NetworkPrefix
  ): Block =
    GenesisProvider
      .construct(
        addresses,
        settings.balanceForEachParticipant,
        settings.initialDifficulty,
        settings.genesisApplicationVersion.blockByte
      )
      .block

}
