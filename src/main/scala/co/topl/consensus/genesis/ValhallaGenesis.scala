package co.topl.consensus.genesis

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.EvidenceProducer.Syntax._
import co.topl.attestation.{Address, PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.consensus.Forger.ChainParams
import co.topl.consensus.genesis.ToplnetGenesis.{blockVersion, genesisAcct, initialDifficulty, log, members, networkPrefix, totalStake}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.modifier.transaction.{ArbitTransfer, PolyTransfer}
import co.topl.settings.{NetworkType, Version}
import co.topl.utils.Int128

import scala.util.Try

case object ValhallaGenesis extends GenesisProvider {

  implicit val networkPrefix: NetworkPrefix = NetworkType.ValhallaTestnet.netPrefix

  override protected val blockChecksum: ModifierId = ModifierId("23iTy3yGats9xDaomWtAX2uyL6pi7XtFMkZ2LaHzoZakj")

  override protected val blockVersion: Version = new Version(1, 0, 0)

  override protected val initialDifficulty: Long = 1000000000000000000L

  override protected val members: Map[String, Int128] = Map(
    "3NL4tpMTMAsDtoye7WWRJh4gDdYADYjt442GRWGN7U8JYCDeURHF" -> 10000000000000000L,
    "3NL4oRjURTwkRjtLmHSdmMfLEq2ABnVNsRaM9xhyvYQD1MmsphEG" -> 10000000000000000L,
    "3NKY8tfVCAL4wD3bmW8zzqb3zRHE4ZoevXihjoopL9s8QcGrDhk9" -> 10000000000000000L,
    "3NKjEMtrNM8AeVCPGco4Tim58uzZjTqPLG6Zjhnfuws96fNEhzyC" -> 10000000000000000L,
    "3NKofLDkjZcDTdx2n9sTTZTcQ7ZAT6CbUDfXGUbg3G7xqg3RzomQ" -> 10000000000000000L,
    "3NLrF3KFKbiMw9ukKkCaYwEjcsYhU54dLvbmLDYMRZzDpFyk9cZD" -> 10000000000000000L,
    "3NLmmqU9smtJC6joiowjgpQUcs4X88REPQmd6sYjdNwgvQBW2WGE" -> 10000000000000000L,
    "3NLt8bVqabahmjUrCRR5qbHuaxwMSohQq8bz1x34KHmmVaYK2k8H" -> 10000000000000000L,
    "3NKb66qf86mhVW6Awwe2GqVHJJGedE3mhK2B5jWUcbMBGxiJSDw7" -> 10000000000000000L,
    "3NKkSTKg4hmi7fTsyBhrpf3NhV5FEf3akap3b6x3btpvJMAuD39G" -> 10000000000000000L,
    "3NL55HVv8bM7vth5BBhUaL4bGpyzTroojAwusaN5wp1LN6ErzmKf" -> 10000000000000000L,
    "3NLKrfiqUEAPkee2et6rUrKzBcxVBKFZzQz6ghzLaKeQjo2xUrpT" -> 10000000000000000L,
    "3NKmMdUJ9QE34u2fotxW9r9seVjGCvJGbuJgP43NkNGd8wjPWXGG" -> 10000000000000000L,
    "3NKexVTP2xYrS64Zh1LeC9gJUhov8vuRXZib3LZhHnhjj123aDiy" -> 10000000000000000L,
    "3NKQmRpo8F5wmZd3RZfSCV3gFfF8nLCRnaFJj7MSi5gssnyi7fr4" -> 10000000000000000L,
    "3NKKaPecSC4F5UHvsVhTmuidfw2zcwuFU5E3WcA1Wq7VhZ1wxjtL" -> 10000000000000000L,
    "3NLKujEoaUfzQ2jErvxMNEomZJQ2wwuyNpDEqfHmBTfLwja4WGht" -> 10000000000000000L,
    "3NKhjnpZ3EFXPNw6WLkCyZ2yhLakkLmsfhBcc7DHuf2Hv2YgLogJ" -> 10000000000000000L,
    "3NLNRgMQEG2W9nAc4mEkTicBBDVxYWZbvLs3jZdpAPjbEQKoBS42" -> 10000000000000000L,
    "3NL5mx6ydpJxH24JwHw3rR8Adetob4XQsLQHvHYP3F89W1943TEX" -> 10000000000000000L,
    )

  def getGenesisBlock: Try[(Block, ChainParams)] = Try {

    val memberKeys = members.keys.map(Address(networkPrefix)(_))

    val txInput = (
      IndexedSeq(),
      memberKeys.zip(members.values.map(SimpleValue(_))).toIndexedSeq,
      Map(genesisAcct.publicImage -> SignatureCurve25519.genesis),
      Int128(0),
      0L,
      None,
      true
    )

    val txs = Seq(
      ArbitTransfer[PublicKeyPropositionCurve25519](
        txInput._1,
        txInput._2,
        txInput._3,
        txInput._4,
        txInput._5,
        txInput._6,
        txInput._7
      ),
      PolyTransfer[PublicKeyPropositionCurve25519](
        txInput._1,
        txInput._2,
        txInput._3,
        txInput._4,
        txInput._5,
        txInput._6,
        txInput._7
      )
    )

    val generatorBox = ArbitBox(genesisAcct.publicImage.generateEvidence, 0, SimpleValue(totalStake))

    val signature = SignatureCurve25519.genesis

    val block =
      Block(
        ModifierId.genesisParentId,
        0L,
        generatorBox,
        genesisAcct.publicImage,
        signature,
        1L,
        initialDifficulty,
        txs,
        blockVersion.blockByte
      )

    require(
      block.id == blockChecksum,
      s"${Console.RED}MALFORMED GENESIS BLOCK! The calculated genesis block " +
        s"with id ${block.id} does not match the required block for the chosen network mode.${Console.RESET}"
    )

    log.debug(s"Initialize state with transaction ${txs.head} with boxes ${txs.head.newBoxes}")

    (block, ChainParams(totalStake, initialDifficulty))
  }
}
