package scorex.testkit

import bifrost.PersistentNodeViewModifier
import bifrost.consensus.{History, SyncInfo}
import bifrost.mempool.MemoryPool
import bifrost.transaction.bifrostTransaction.Transaction
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.MinimalState
import scorex.testkit.properties._

/**
  * Performance test for implementations
  */
trait BlockchainPerformance[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool],
ST <: MinimalState[P, B, TX, PM, ST],
HT <: History[P, TX, PM, SI, HT]] extends MempoolFilterPerformanceTest[P, TX, MPool]
