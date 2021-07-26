package co.topl.consensus

import co.topl.utils.NetworkType.{NetworkPrefix, ValhallaTestnet}

/**
 * Represents the sets of blocks that should have failed to be added to networks, but were anyways.
 */
object Hiccups {

  /**
   * Represents a block that passed validation at some point in a chain when it should not have.
   * @param id the ID of the block
   * @param height the height of the block
   */
  case class HiccupBlock(id: String, height: Long, networkPrefix: NetworkPrefix)

  /**
   * Represent hiccups with semantic validation that should be ignored in node view holder checks.
   */
  val semanticValidation: Seq[HiccupBlock] = Seq(
    HiccupBlock(
      "29QHPjqyLB1QN6DhArf125Nu3qfgKLcPRnZGvaCX8qDNf",
      255181,
      ValhallaTestnet.netPrefix
    ),
    HiccupBlock(
      "2AsEgm1548vbwos8qqfe1qwwBF6Ef1mzKnRhPvZpALM2A",
      262558,
      ValhallaTestnet.netPrefix
    ),
    HiccupBlock(
      "293EqLkRWEEjV8aW99w4xXeyescvriYXytHdSn7LudSd1",
      262875,
      ValhallaTestnet.netPrefix
    )
  )

  /**
   * Represents hiccups with block validation that should be ignored in history appends.
   */
  val blockValidation: Seq[HiccupBlock] = Seq(
    // Valhalla
    HiccupBlock(
      "21oHt9kQaKRn5wnTvBPG1wUaeUGAVBKPQMbh1ZjFqviu4",
      928865,
      ValhallaTestnet.netPrefix
    ),
    // Valhalla
    HiccupBlock(
      "xNtkkMJd3oHd9W8iHdrNoJR8KZ6KdZYDUTh7rWeBkgtE",
      929061,
      ValhallaTestnet.netPrefix
    ),
  )
}
