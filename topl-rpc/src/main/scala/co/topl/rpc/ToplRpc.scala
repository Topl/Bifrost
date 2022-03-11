package co.topl.rpc

import cats.data.NonEmptyChain
import co.topl.akkahttprpc.Rpc
import co.topl.attestation.{Address, Proposition}
import co.topl.models.{DionAddress, Int128 => TetraInt128, Transaction => TetraTransaction, TransactionData}
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.box.AssetCode.AssetCodeVersion
import co.topl.modifier.box._
import co.topl.modifier.transaction.Transaction.TX
import co.topl.modifier.transaction.builder.BoxSelectionAlgorithm
import co.topl.modifier.transaction.{ArbitTransfer, AssetTransfer, PolyTransfer}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

object ToplRpc {

  object Debug {

    object Delay {

      /**
       * Calculate the average delay over a number of blocks
       *
       * Find the average delay between blocks starting from a specified blockId and till a certain number of blocks
       * forged on top of it
       */
      val rpc: Rpc[Params, Response] = Rpc("debug_delay")

      /**
       * @param blockId Id of block from which to start average delay computation
       * @param numBlocks Number of blocks back to consider when computing average delay
       */
      case class Params(blockId: ModifierId, numBlocks: Int)
      case class Response(delay: String)
    }

    object MyBlocks {

      /**
       * Find the number of blocks forged by addresses held by the node
       *
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       */
      val rpc: Rpc[Params, Response] = Rpc("debug_myBlocks")
      case class Params()
      case class Response(pubkeys: Set[Address], count: Int)
    }

    object Generators {

      /**
       * Find distribution of block generators from all addresses in the chain's history
       */
      val rpc: Rpc[Params, Response] = Rpc("debug_generators")
      case class Params()
      type Response = Map[Address, Int]
    }

    object IdsFromHeight {

      /**
       * Return all block ids from a given height and down to a given limit
       */
      val rpc: Rpc[Params, Response] = Rpc("debug_idsFromHeight")
      case class Params(height: Long, limit: Int)
      type Response = List[ModifierId]
    }
  }

  object Util {

    object Seed {

      /**
       * Generates random seed of 32 bytes
       */
      val rpc: Rpc[Params, Response] = Rpc("util_seed")
      case class Params()
      case class Response(seed: String)
    }

    object SeedOfLength {

      /**
       * Generates random seed of specified length
       */
      val rpc: Rpc[Params, Response] = Rpc("util_seedOfLength")

      /**
       * @param length The number of characters to return
       */
      case class Params(length: Int)
      case class Response(seed: String)
    }

    object HashBlake2b256 {

      /**
       * Returns Blake2b hash of specified message
       */
      val rpc: Rpc[Params, Response] = Rpc("util_hashBlake2b256")

      /**
       * @param message The message that will be hashed
       */
      case class Params(message: String)
      case class Response(message: String, hash: String)
    }

    object GenerateAssetCode {

      /**
       * Returns an encoded assetCode generated from provided parameters
       */
      val rpc: Rpc[Params, Response] = Rpc("util_generateAssetCode")

      /**
       * @param version AssetCode version(version 1 would be string "1")
       * @param issuer The Address of the asset issuer
       * @param shortName A Latin-1 encoded string of up to 8 characters
       */
      case class Params(version: AssetCodeVersion, issuer: Address, shortName: String)
      case class Response(assetCode: AssetCode)
    }

    object CheckValidAddress {

      /**
       * Check if the provided address is valid, returns the address and network type
       */
      val rpc: Rpc[Params, Response] = Rpc("util_checkValidAddress")

      /**
       * @param network A Latin-1 encoded string of up to 8 characters
       * @param address The Address of the asset issuer
       */
      case class Params(network: Option[String], address: String)
      case class Response(address: Address, network: String)
    }
  }

  object NodeView {

    object Head {

      /**
       * Retrieve the best block
       *
       * Find information about the current state of the chain including height, score, bestBlockId, etc
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_head")

      case class Params()
      case class Response(height: Int128, score: Long, bestBlockId: ModifierId, bestBlock: Block)
    }

    object HeadInfo {

      /**
       * Retrieve the best block's id and other info
       *
       * Find information about the current state of the chain including height and bestBlockId
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_headInfo")

      case class Params()
      case class Response(bestBlockId: ModifierId, height: Int128)
    }

    object Balances {

      /**
       * Lookup balances
       *
       * Remote -- Transaction must be used in conjunction with an external key manager service.
       *
       * Requires the Token Box Registry to be active
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_balances")

      /**
       * @param addresses Addresses whose balances are to be retrieved
       */
      case class Params(addresses: List[Address])
      type Response = Map[Address, Entry]
      case class Entry(Balances: EntryBalances, Boxes: EntryBoxes)
      case class EntryBalances(Polys: Int128, Arbits: Int128)
      case class EntryBoxes(PolyBox: List[PolyBox], ArbitBox: List[ArbitBox], AssetBox: List[AssetBox])
    }

    object TransactionById {

      /**
       * Lookup a transaction by its id
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_transactionById")

      /**
       * Base58 encoded transaction hash
       */
      case class Params(transactionId: ModifierId)

      case class Response(
        transaction: TX,
        blockNumber: Long,
        blockId:     ModifierId
      )
    }

    object BlockById {

      /**
       * Lookup a block by its id
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_blockById")

      /**
       * @param blockId Base58 encoded transaction hash
       */
      case class Params(blockId: ModifierId)
      type Response = Block
    }

    object BlocksByIds {

      /**
       * Lookup a block by its id
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_blocksByIds")

      /**
       * @param blockIds Base58 encoded transaction hash
       */
      case class Params(blockIds: List[ModifierId])
      type Response = List[Block]
    }

    object BlocksInRange {

      /**
       * Retrieve a segment of the chain in a height range
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_blocksInRange")

      /**
       * @param startHeight starting height for the segment of chain
       * @param endHeight end heigh for the segment of chain
       */
      case class Params(startHeight: Long, endHeight: Long)
      type Response = List[Block]
    }

    object BlockIdsInRange {

      /**
       * Retrieve the ids of a segment of the chain in a height range
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_blockIdsInRange")

      /**
       * @param startHeight starting height for the segment of chain
       * @param endHeight end heigh for the segment of chain
       */
      case class Params(startHeight: Long, endHeight: Long)
      type Response = List[ModifierId]
    }

    object LatestBlocks {

      /**
       * Retrieve a number of latest blocks, including the current best block
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_latestBlocks")

      /**
       * @param numberOfBlocks number of latest blocks to retrieve
       */
      case class Params(numberOfBlocks: Int)
      type Response = List[Block]
    }

    object LatestBlockIds {

      /**
       * Retrieve a number of latest blocks' ids, including the current best block id
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_latestBlockIds")

      /**
       * @param numberOfBlockIds number of latest blocks to retrieve
       */
      case class Params(numberOfBlockIds: Int)
      type Response = List[ModifierId]
    }

    object BlockByHeight {

      /**
       * Lookup a block by its height
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_blockByHeight")

      /**
       * @param height Height to retrieve on the canonical chain
       */
      case class Params(height: Long)
      type Response = Block
    }

    object Mempool {

      /**
       * Get the first 100 transactions in the mempool (sorted by fee amount)
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_mempool", aliases = List("topl_getPendingTransactions"))

      case class Params()
      type Response = List[TX]
    }

    object TransactionFromMempool {

      /**
       * Lookup a transaction in the mempool by its id
       */
      val rpc: Rpc[Params, Response] =
        Rpc("topl_transactionFromMempool", aliases = List("topl_getPendingTransactionById"))

      /**
       * @param transactionId Base58 encoded transaction hash
       */
      case class Params(transactionId: ModifierId)

      type Response = TX
    }

    object ConfirmationStatus {

      /**
       * Lookup the confirmation status of transactions
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_confirmationStatus")

      /**
       * @param transactionIds Base58 encoded transaction hash
       */
      case class Params(transactionIds: List[ModifierId])
      type Response = Map[ModifierId, TxStatus]
      case class TxStatus(status: String, depthFromHead: Long)
    }

    object Info {

      /**
       * Retrieve information about this running node
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_info")

      case class Params()

      case class Response(
        network:                String,
        nodeAddress:            String,
        appVersion:             String,
        currentProtocolRuleset: String,
        currentBlockVersion:    String
      )
    }
  }

  object Transaction {

    object RawAssetTransfer {

      /**
       * #### Summary
       * Transfer Assets from an account to a specified recipient
       *
       * #### Type
       * Remote -- Transaction must be used in conjunction with an external key manager service.
       *
       * #### Description
       * Default behavior of the wallet is to find the first unlocked address which hold the targetted Asset.
       * The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
       *
       * #### Notes
       * - `AssetCode` in `AssetValue` can be generated using `util_generateAssetCode`
       * - `fee` and `quantity` in `AssetValue` need to be strings, they will be converted into Int128 which can go up
       * to 178 undecillion(2^127-1)
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_rawAssetTransfer")

      /**
       * @param propositionType Type of proposition, eg., PublicKeyCurve25519, ThresholdCurve25519
       * @param sender Array of addresses from which Assets should be sent
       * @param recipients Array of addresses and assetValues for the transfer recipients
       * @param fee Fee for the transfer. Minting AssetTransfer requires fee to be greater than 0
       * @param changeAddress Address for recipient of unspent Polys
       * @param consolidationAddress Address for recipient of unspent Assets
       * @param minting If this is a minting AssetTransfer or not
       * @param data Data string which can be associated with this transaction(may be empty)
       * @param boxSelectionAlgorithm Algorithm for selecting which boxes should be included in the Asset Transfer.
       *                              The default value is `All`.
       */
      case class Params(
        propositionType:       String,
        sender:                NonEmptyChain[Address],
        recipients:            NonEmptyChain[(Address, AssetValue)],
        fee:                   Int128,
        changeAddress:         Address,
        consolidationAddress:  Address,
        minting:               Boolean,
        data:                  Option[Latin1Data],
        boxSelectionAlgorithm: BoxSelectionAlgorithm
      )

      case class Response(rawTx: AssetTransfer[Proposition], messageToSign: String)
    }

    object RawArbitTransfer {

      /**
       * #### Summary
       * Transfer Arbits from an account to a specified recipient.
       *
       * #### Type
       * Remote -- Transaction must be used in conjunction with an external key manager service.
       *
       * #### Description
       * Default behavior of the wallet is to find the first unlocked address which hold Arbits.
       * The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
       *
       * #### Notes
       * - `fee` and Arbit amounts in `recipients` need to be strings, they will be converted into Int128 which can go
       * up to 178 undecillion(2^127-1)
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_rawArbitTransfer")

      /**
       * @param propositionType Type of proposition, eg., PublicKeyCurve25519, ThresholdCurve25519
       * @param sender Array of addresses from which Arbits should be sent
       * @param recipients Array of addresses and Arbit amounts for the corresponding recipients
       * @param fee Fee for the transfer. Minting requires fee to be greater than 0
       * @param changeAddress Address for recipient of unspent Arbits
       * @param consolidationAddress Address for recipient of unspent Arbits
       * @param data Data string which can be associated with this transaction(may be empty)
       * @param boxSelectionAlgorithm Algorithm for selecting which boxes should be included in the Arbit Transfer.
       *                              The default value is `All`.
       */
      case class Params(
        propositionType:       String,
        sender:                NonEmptyChain[Address],
        recipients:            NonEmptyChain[(Address, Int128)],
        fee:                   Int128,
        changeAddress:         Address,
        consolidationAddress:  Address,
        data:                  Option[Latin1Data],
        boxSelectionAlgorithm: BoxSelectionAlgorithm
      )

      case class Response(rawTx: ArbitTransfer[Proposition], messageToSign: String)
    }

    object RawPolyTransfer {

      /**
       * #### Summary
       * Transfer Polys from an account to a specified recipient.
       *
       * #### Type
       * Remote -- Transaction must be used in conjunction with an external key manager service.
       *
       * #### Description
       * Default behavior of the wallet is to find the first unlocked address which hold Polys.
       * The protocols default behavior is to combine multiple UTXOs of the same type into a single UTXO when it can.
       *
       * #### Notes
       * - `fee` and Poly amounts in `recipients` need to be strings, they will be converted into Int128 which can go up
       * to 178 undecillion(2^127-1)
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_rawPolyTransfer")

      /**
       * @param propositionType Type of proposition, eg., PublicKeyCurve25519, ThresholdCurve25519
       * @param sender Array of addresses from which Polys should be sent
       * @param recipients Array of addresses and Poly amounts for the corresponding recipients
       * @param fee Fee for the transfer. Minting AssetTransfer requires fee to be greater than 0
       * @param changeAddress Address for recipient of unspent Polys
       * @param data Data string which can be associated with this transaction(may be empty)
       * @param boxSelectionAlgorithm Algorithm for selecting which boxes should be included in the Poly Transfer.
       *                              The default value is `All`.
       */
      case class Params(
        propositionType:       String,
        sender:                NonEmptyChain[Address],
        recipients:            NonEmptyChain[(Address, Int128)],
        fee:                   Int128,
        changeAddress:         Address,
        data:                  Option[Latin1Data],
        boxSelectionAlgorithm: BoxSelectionAlgorithm
      )

      case class Response(rawTx: PolyTransfer[Proposition], messageToSign: String)
    }

    object UnprovenPolyTransfer {

      val rpc: Rpc[Params, Response] = Rpc("topl_unprovenPolyTransfer")

      case class Params(
        sender:                NonEmptyChain[DionAddress],
        recipients:            NonEmptyChain[TetraTransaction.PolyOutput],
        fee:                   TetraInt128,
        changeAddress:         DionAddress,
        data:                  Option[TransactionData],
        boxSelectionAlgorithm: BoxSelectionAlgorithm
      )

      case class Response(unprovenTransfer: TetraTransaction.Unproven)
    }

    object BroadcastTx {

      /**
       * #### Summary
       * Broadcast transaction
       *
       * #### Type
       * Remote -- Route must be used in conjunction with an external key manager service.
       *
       * #### Description
       * Place specified signed transaction into the mempool and broadcast to other nodes
       *
       * #### Notes
       * - Currently only enabled for `AssetCreation` and `AssetTransfer` transactions
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_broadcastTx")

      /**
       * @param tx A full formatted transaction JSON object (prototype transaction + signatures)
       */
      case class Params(tx: TX)

      type Response = TX
    }

    object BroadcastTetraTransfer {

      val rpc: Rpc[Params, Response] = Rpc("topl_broadcastTetraTransfer")

      case class Params(transfer: TetraTransaction)

      type Response = TX
    }

    object EncodeTransfer {

      /**
       * #### Summary
       * Encode unsigned transfer
       *
       * #### Type
       * Remote -- Route must be used in conjunction with an external key manager service.
       *
       * #### Description
       * Encode an unsigned transfer into the messageToSign data
       */
      val rpc: Rpc[Params, Response] = Rpc("topl_encodeTransfer")

      /**
       * @param unprovenTransaction An unsigned transaction JSON
       */
      case class Params(unprovenTransaction: TX)

      case class Response(messageToSign: String)
    }
  }

  object Admin {

    object UnlockKeyfile {

      /**
       * #### Summary
       * Unlock keyfile
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Unlock an encrypted keyfile which exists in your keyfile directory. This will add the secret key to wallet and
       * allow signing of transactions on behalf of that key
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_unlockKeyfile")

      /**
       * @param address Address corresponding to an encrypted keyfile in your wallet directory
       * @param password String used to encrypt the private keyfile that is stored locally
       */
      case class Params(address: String, password: String)
      type Response = Map[Address, String]
    }

    object LockKeyfile {

      /**
       * #### Summary
       * Lock keyfile
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Lock a previously unlocked keyfile in your wallet.
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_lockKeyfile")

      /**
       * @param address Address corresponding to an encrypted keyfile in your wallet directory
       */
      case class Params(address: Address)
      type Response = Map[Address, String]
    }

    object GenerateKeyfile {

      /**
       * #### Summary
       * Generate a new keyfile in local storage
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Generate and save a new encrypted private keyfile using Curve25519 key pairs.
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_generateKeyfile")

      /**
       * @param password String used to encrypt the private keyfile that is stored locally
       */
      case class Params(password: String)
      case class Response(address: Address)
    }

    object ImportSeedPhrase {

      /**
       * #### Summary
       * Import key from mnemonic
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Allows a user to import a 12, 15, 18, 21, or 24 word mnemonic (seed phrase) and generate an encrypted Keyfile
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_importSeedPhrase")

      /**
       * @param password String used to encrypt the private keyfile that is stored locally
       * @param seedPhrase 12, 15, 18, 21, or 24 word mnemonic
       * @param seedPhraseLang Defaults to 'en'. Valid options are ["zh-hans", "zh-hant", "en", "fr", "it", "ja", "ko", "es"]
       */
      case class Params(password: String, seedPhrase: String, seedPhraseLang: String = "en")
      case class Response(publicKey: Address)
    }

    /**
     * #### Summary
     * Return list of open keyfiles
     *
     * #### Type
     * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
     *
     * #### Description
     * Check which keyfiles are currently unlocked in your wallet. This method takes no input arguments.
     */
    object ListOpenKeyfiles {
      val rpc: Rpc[Params, Response] = Rpc("admin_listOpenKeyfiles")

      case class Params()
      case class Response(unlocked: Set[Address])
    }

    /**
     * #### Summary
     * Send the start forging signal
     *
     * #### Type
     * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
     *
     * #### Description
     * Attempt to forge blocks using any unlocked keyfiles available on the node
     */
    object StartForging {
      val rpc: Rpc[Params, Response] = Rpc("admin_startForging")

      case class Params()
      case class Response(msg: String)
    }

    object StopForging {

      /**
       * #### Summary
       * Send the stop forging signal
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Attempt to stop forging blocks
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_stopForging")

      case class Params()
      case class Response(msg: String)
    }

    object UpdateRewardsAddress {

      /**
       * #### Summary
       * Allows the user to specify a new PublicKey address to receive block rewards
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Change the address used to receive block rewards. This method requires the new address as a string
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_updateRewardsAddress")

      /**
       * @param address New address to receive block rewards
       */
      case class Params(address: Address)
      case class Response(msg: String)
    }

    object GetRewardsAddress {

      /**
       * #### Summary
       * Return list of open keyfiles
       *
       * #### Type
       * Local Only -- An unlocked keyfile must be accessible (in local storage) to fulfill this request
       *
       * #### Description
       * Check which keyfiles are currently unlocked in your wallet. This method takes no input arguments.
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_getRewardsAddress")

      case class Params()
      case class Response(rewardsAddress: String)
    }

    object Status {

      /**
       * Retrieve information about this running node
       */
      val rpc: Rpc[Params, Response] = Rpc("admin_status")

      case class Params()
      case class Response(forgingStatus: String, numberOfPendingTransactions: Int)
    }
  }

}
