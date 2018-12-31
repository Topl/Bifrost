package scorex.testkit

import bifrost.PersistentNodeViewModifier
import bifrost.consensus.{History, SyncInfo}
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.MinimalState
import bifrost.transaction.{MemoryPool, Transaction}
import scorex.testkit.properties._

/**
  * The idea of this class is to get some generators and test some situations, common for all blockchains
  */
trait BlockchainSanity[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
B <: Box[P],
MPool <: MemoryPool[TX, MPool],
ST <: MinimalState[P, B, TX, PM, ST],
HT <: History[P, TX, PM, SI, HT]] extends HistoryAppendBlockTest[P, TX, PM, SI, HT]
  with StateApplyChangesTest[P, TX, PM, B, ST]
  with WalletSecretsTest[P, TX, PM]
  with StateRollbackTest[P, TX, PM, B, ST]
  with MempoolTransactionsTest[P, TX, MPool]
  with MempoolFilterPerformanceTest[P, TX, MPool]
  with StateChangesGenerationTest[P, TX, PM, B, ST, SI, HT] {


}
