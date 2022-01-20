package co.topl

package object consensus {

  // Initialize or restore a consensus storage that keeps track of the maxStake, difficulty, height, and inflation
  private[topl] var consensusStorage: ConsensusStorage = ConsensusStorage.emptyStorage()
}
