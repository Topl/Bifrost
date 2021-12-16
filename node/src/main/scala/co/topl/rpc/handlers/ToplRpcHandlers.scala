package co.topl.rpc.handlers

import co.topl.rpc.ToplRpc

case class ToplRpcHandlers(
  debug:       ToplRpcHandlers.Debug,
  utils:       ToplRpcHandlers.Utils,
  nodeView:    ToplRpcHandlers.NodeView,
  transaction: ToplRpcHandlers.Transaction,
  admin:       ToplRpcHandlers.Admin
)

object ToplRpcHandlers {

  trait Debug {
    def delay: ToplRpc.Debug.Delay.rpc.ServerHandler
    def myBlocks: ToplRpc.Debug.MyBlocks.rpc.ServerHandler
    def generators: ToplRpc.Debug.Generators.rpc.ServerHandler
    def idsFromHeight: ToplRpc.Debug.IdsFromHeight.rpc.ServerHandler
  }

  trait Utils {
    def seed: ToplRpc.Util.Seed.rpc.ServerHandler
    def seedOfLength: ToplRpc.Util.SeedOfLength.rpc.ServerHandler
    def hashBlake2b256: ToplRpc.Util.HashBlake2b256.rpc.ServerHandler
    def generateAssetCode: ToplRpc.Util.GenerateAssetCode.rpc.ServerHandler
    def checkValidAddress: ToplRpc.Util.CheckValidAddress.rpc.ServerHandler
  }

  trait NodeView {
    def head: ToplRpc.NodeView.Head.rpc.ServerHandler
    def headInfo: ToplRpc.NodeView.HeadInfo.rpc.ServerHandler
    def balances: ToplRpc.NodeView.Balances.rpc.ServerHandler
    def transactionById: ToplRpc.NodeView.TransactionById.rpc.ServerHandler
    def blockById: ToplRpc.NodeView.BlockById.rpc.ServerHandler
    def blocksByIds: ToplRpc.NodeView.BlocksByIds.rpc.ServerHandler
    def blockByHeight: ToplRpc.NodeView.BlockByHeight.rpc.ServerHandler
    def blocksInRange: ToplRpc.NodeView.BlocksInRange.rpc.ServerHandler
    def blockIdsInRange: ToplRpc.NodeView.BlockIdsInRange.rpc.ServerHandler
    def latestBlocks: ToplRpc.NodeView.LatestBlocks.rpc.ServerHandler
    def latestBlockIds: ToplRpc.NodeView.LatestBlockIds.rpc.ServerHandler
    def mempool: ToplRpc.NodeView.Mempool.rpc.ServerHandler
    def transactionFromMempool: ToplRpc.NodeView.TransactionFromMempool.rpc.ServerHandler
    def confirmationStatus: ToplRpc.NodeView.ConfirmationStatus.rpc.ServerHandler
    def info: ToplRpc.NodeView.Info.rpc.ServerHandler
    def status: ToplRpc.NodeView.Status.rpc.ServerHandler
  }

  trait Transaction {
    def rawAssetTransfer: ToplRpc.Transaction.RawAssetTransfer.rpc.ServerHandler
    def rawArbitTransfer: ToplRpc.Transaction.RawArbitTransfer.rpc.ServerHandler
    def rawPolyTransfer: ToplRpc.Transaction.RawPolyTransfer.rpc.ServerHandler
    def broadcastTx: ToplRpc.Transaction.BroadcastTx.rpc.ServerHandler
    def encodeTransfer: ToplRpc.Transaction.EncodeTransfer.rpc.ServerHandler
  }

  trait Admin {
    def unlockKeyfile: ToplRpc.Admin.UnlockKeyfile.rpc.ServerHandler
    def lockKeyfile: ToplRpc.Admin.LockKeyfile.rpc.ServerHandler
    def generateKeyfile: ToplRpc.Admin.GenerateKeyfile.rpc.ServerHandler
    def importSeedPhrase: ToplRpc.Admin.ImportSeedPhrase.rpc.ServerHandler
    def listOpenKeyfiles: ToplRpc.Admin.ListOpenKeyfiles.rpc.ServerHandler
    def startForging: ToplRpc.Admin.StartForging.rpc.ServerHandler
    def stopForging: ToplRpc.Admin.StopForging.rpc.ServerHandler
    def updateRewardsAddress: ToplRpc.Admin.UpdateRewardsAddress.rpc.ServerHandler
    def getRewardsAddress: ToplRpc.Admin.GetRewardsAddress.rpc.ServerHandler
  }
}
