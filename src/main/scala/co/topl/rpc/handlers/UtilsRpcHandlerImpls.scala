package co.topl.rpc.handlers

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{InvalidParametersError, RpcError, ThrowableData}
import co.topl.attestation.AddressEncoder
import co.topl.modifier.box.AssetCode
import co.topl.rpc.{ToplRpc, ToplRpcErrors}
import co.topl.utils.NetworkType
import co.topl.utils.NetworkType.NetworkPrefix
import io.circe.{DecodingFailure, Encoder}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58

import java.security.SecureRandom
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

class UtilsRpcHandlerImpls(implicit
  throwableEncoder: Encoder[ThrowableData],
  ec:               ExecutionContext,
  networkPrefix:    NetworkPrefix
) extends ToplRpcHandlers.Utils {

  import UtilsRpcHandlerImpls._

  val defaultSeedSize = 32 // todo: JAA - read this from a more appropriate place. Bip39 spec or something?

  override val seed: ToplRpc.Util.Seed.rpc.ServerHandler =
    _ => EitherT.pure[Future, RpcError[_]](ToplRpc.Util.Seed.Response(generateSeed(defaultSeedSize)))

  override val seedOfLength: ToplRpc.Util.SeedOfLength.rpc.ServerHandler =
    params => EitherT.pure[Future, RpcError[_]](ToplRpc.Util.SeedOfLength.Response(generateSeed(params.length)))

  override val hashBlake2b256: ToplRpc.Util.HashBlake2b256.rpc.ServerHandler =
    params =>
      EitherT.pure[Future, RpcError[_]](
        ToplRpc.Util.HashBlake2b256.Response(params.message, Base58.encode(Blake2b256(params.message)))
      )

  override val generateAssetCode: ToplRpc.Util.GenerateAssetCode.rpc.ServerHandler =
    params =>
      EitherT.fromEither[Future](
        Try(AssetCode(params.version, params.issuer, params.shortName)).toEither
          .leftMap(ToplRpcErrors.FailedToGenerateAssetCode)
          .map(ToplRpc.Util.GenerateAssetCode.Response)
      )

  override val checkValidAddress: ToplRpc.Util.CheckValidAddress.rpc.ServerHandler =
    params =>
      EitherT.fromEither[Future](
        params.network
          .fold(NetworkType.pickNetworkType(networkPrefix))(NetworkType.pickNetworkType)
          .toRight(ToplRpcErrors.InvalidNetworkSpecified)
          .flatMap(nt =>
            AddressEncoder
              .fromStringWithCheck(params.address, nt.netPrefix)
              .leftMap(e => InvalidParametersError(DecodingFailure(e.toString, Nil)))
              .map(address => ToplRpc.Util.CheckValidAddress.Response(address, nt.verboseName))
          )
      )
}

object UtilsRpcHandlerImpls {

  private def generateSeed(length: Int): String = {
    val seed = new Array[Byte](length)
    new SecureRandom().nextBytes(seed) //seed mutated here!
    Base58.encode(seed)
  }
}
