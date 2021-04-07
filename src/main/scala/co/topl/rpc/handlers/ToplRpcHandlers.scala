package co.topl.rpc.handlers

import co.topl.rpc.ToplRpc

case class ToplRpcHandlers(
  debug: ToplRpcHandlers.Debug,
  utils: ToplRpcHandlers.Utils
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
}
