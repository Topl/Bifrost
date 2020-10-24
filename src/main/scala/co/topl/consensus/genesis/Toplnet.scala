package co.topl.consensus.genesis

import co.topl.consensus.Forger.ChainParams
import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.crypto.signature.Signature25519
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.{ ArbitTransfer, PolyTransfer }
import co.topl.nodeView.history.History
import co.topl.nodeView.state.box.ArbitBox
import co.topl.settings.Version

import scala.util.Try

case object Toplnet extends GenesisProvider {

  override protected val blockChecksum: ModifierId = ModifierId("9VX9smBd7Jz56HzTcmY6EZiLfrn7WdxECbsSgNRrPXmu")

  override protected val blockVersion: Version = new Version(0, 0, 1)

  override protected val initialDifficulty: Long = 1000000000000000000L

  //propositions with wallet seed genesisoo, genesiso1, ..., genesis48, genesis49
  override protected val members: Map[String, Long] = Map(
    "6sYyiTguyQ455w2dGEaNbrwkAWAEYV1Zk6FtZMknWDKQ" -> 100000000L,
    "Ei8oY3eg5vM26QUBhyFiAdPN1C23RJEV9irrykNmSAFV" -> 100000000L,
    "EakiCSw1rfmL5DFTPNmSJZEEAEGtTp3DN12wVPJVsURS" -> 100000000L,
    "DSL6bvb6j1v6SnvKjqc6fJWdsRjZ85YboH8FkzonUPiT" -> 100000000L,
    "GydWCS1GwExoDNuEiW6fBLYr7cs4vwdLpk1kzDeKHq6A" -> 100000000L,
    "9E4F53GSXMPqwuPWEVoUQe9B1z4A8v9Y6tAQdKK779km" -> 100000000L,
    "8Sp3v5vtYtkM9Z2K2B7PuZbWmWQE9bfiUFCvkmsdauGj" -> 100000000L,
    "ftqJXjSXrWQXmumNVVaRiNB7TZuCy4GCvz9V4GJGhAv"  -> 100000000L,
    "3nFprwUuqGH9BpvJMQeCb5AwHdaXuxKin1WSxWc9PTkY" -> 100000000L,
    "EPbo8xRWARg2znJAqevKnQMskxnemmCdimPiVFhr8eLd" -> 100000000L,
    "52gwahUytUXv7wfKs4j6YeKeepc38sYsUi4jp4z4jVym" -> 100000000L,
    "G1yK5iwPQKNXnqU4Drg83et3gKhRW5CogqiekKEYDcrt" -> 100000000L,
    "3FAskwxrbqiX2KGEnFPuD3z89aubJvvdxZTKHCrMFjxQ" -> 100000000L,
    "7R9waVeAKuHKNQY5uTYBp6zNLNo6wSDvj9XfQCyRWmDF" -> 100000000L,
    "AEkuiLFdudYmUwZ9dSa64rakqUgJZf6pKFFwwm6CZFQz" -> 100000000L,
    "EjpGvdZETt3SuZpcuwKvZS4jgWCockDHzFQLoeYNW4R"  -> 100000000L,
    "8V5y1CSC1gCGD1jai3ns5FJNW7tAzf7BGd4iwmBv7V44" -> 100000000L,
    "7eboeRCeeBCFwtzPtB4vKPnaYMPL52BjfiEpqSRWfkgx" -> 100000000L,
    "9PLHPwnHyA5jf6GPGRjJt7HNd93rw4gWTBi7LBNL4Wwt" -> 100000000L,
    "3oTzYXjwdr684FUzaJEVVuXBztysNgR8M8iV9QykaM9C" -> 100000000L,
    "4wtQpa1BVgAt9CA4FUuHZHCYGBYtvudPqa1sAddfAPii" -> 100000000L,
    "6BtXEZE6GcxtEtSLAHXkE3mkcTG1u8WuoQxZG7R8BR5X" -> 100000000L,
    "7focbpSdsNNE4x9h7eyXSkvXE6dtxsoVyZMpTpuThLoH" -> 100000000L,
    "CfvbDC8dxGeLXzYhDpNpCF2Ar9Q5LKs8QrfcMYAV59Lt" -> 100000000L,
    "FuTHJNKaPTneEYRkjKAC3MkSttvAC7NtBeb2uNGS8mg3" -> 100000000L,
    "7BDhJv6Wh2MekgJLvQ98ot9xiw5x3N4b3KipURdrW8Ge" -> 100000000L,
    "8LNhm5QagL88sWggvJKGDiZ5bBCG4ajV7R6vAKz4czA9" -> 100000000L,
    "AEQ8bZRuAxAp8DV9VZnTrSudGPdNyzY2HXjPBCGy8igf" -> 100000000L,
    "419sTmWKAXb5526naQ93xJZL4YAYtpVkbLmzMb6k5X9m" -> 100000000L,
    "G8xVDYow1YcSb4cuAHwcpYSEKxFpYwC9GqYChMvbCWn5" -> 100000000L,
    "5XtHBDxXCudA38FJnoWm1BVG8aV67AiQKnPuwYbWZCb3" -> 100000000L,
    "8XTUXeLiHPbMNXedWQh5xHQtq4xUHU3pZZGqRQzC2eyj" -> 100000000L,
    "GMAYWvbBmssCr55m9bcq8cKzfczSKKxidtVrukBM1KFN" -> 100000000L,
    "HfYNA96cGebFGgAhGUbxvRJYyLFchQJZpJTQMXztE6gZ" -> 100000000L,
    "4pygr1SPEe5KbU1R8XgMmYaW7YfTH818wd113mF6bhsP" -> 100000000L,
    "Hi3Q1ZQbD2zztq6ajm5yUKfFccxmj3yZn79GUjhFvPSW" -> 100000000L,
    "Hf8XcEAVMCiWbu376rGS48FhwH5NgteivfsTsvX1XpbA" -> 100000000L,
    "GgahaaNBaHRnyUtvEu3k7N5BnW3dvhVCXyxMP6uijdhh" -> 100000000L,
    "E4AoFDANgDFL83gTS6A7kjWbLmqWcPr6DqEgMG7cqU18" -> 100000000L,
    "3QzGZvvTQbcUdhd5BL9ofEK3GdzbmqUnYA1pYTAdVY44" -> 100000000L,
    "C85c1uMAiHKDgcqxF6EaiCGQyWgQEYATbpo8M7XEnx3R" -> 100000000L,
    "CJ9udTDT61ckSHMd6YNpjeNdsN2fGwmJ6Ry6YERXmGa7" -> 100000000L,
    "E3JJCTMouTys5BSwFyHTV3Ht55mYWfNUAverrNaVo4jE" -> 100000000L,
    "2YM2FQ4HfMiV3LFkiwop2xFznbPVEHbhahVvcrhfZtXq" -> 100000000L,
    "J6bgGpwDMqKFrde2mpdS6dasRyn9WFV6jKgWAkHSN91q" -> 100000000L,
    "DaSXwzkAU2WfH39zxMfuXpExsVfKk6JzeYbdW9RLiXr4" -> 100000000L,
    "39Z9VaCAeqoWajHyku29argf7zmVqs2vVJM8zYe7YLXy" -> 100000000L,
    "CBdnTL6C4A7nsacxCP3VL3TqUokEraFy49ckQ196KU46" -> 100000000L,
    "GFseSi5squ8GRRkj6RknbGj9Hyz82HxKkcn8NKW1e5CF" -> 100000000L,
    "5hhPGEFCZM2HL6DNKs8KvUZAH3wC47rvMXBGftw9CCA5" -> 100000000L
    )

  def getGenesisBlock: Try[(Block, ChainParams)] = Try {

    val memberKeys = members.keys.map(PublicKey25519Proposition.apply)

    val txInput = (
      IndexedSeq(genesisAcct.publicImage -> 0L),
      memberKeys.zip(members.values).toIndexedSeq,
      Map(genesisAcct.publicImage -> Signature25519.genesis()),
      0L,
      0L,
      "")

    val txs = Seq((ArbitTransfer.apply: ARB).tupled(txInput), (PolyTransfer.apply: POLY).tupled(txInput))

    val generatorBox = ArbitBox(genesisAcct.publicImage, 0, totalStake)

    val signature = Signature25519.genesis()

    val block = Block(History.GenesisParentId, 0L, generatorBox, signature, txs, blockVersion.blockByte)

    require(block.id == blockChecksum, s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
      s"with id ${block.id} does not match the required block for the chosen network mode.${Console.RESET}")

    log.debug(s"Initialize state with transaction ${txs.head} with boxes ${txs.head.newBoxes}")

    (block, ChainParams(totalStake, initialDifficulty))
  }
}
