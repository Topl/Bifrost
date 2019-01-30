package bifrost.api

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.model.{HttpEntity, HttpMethods, HttpRequest, MediaTypes}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.pattern.ask
import akka.util.{ByteString, Timeout}
import bifrost.api.http.{NodeViewApiRouteRPC, UtilsApiRouteRPC}
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import bifrost.{BifrostGenerators, BifrostNodeViewHolder}
import io.circe.Json
import io.circe.parser.parse
import org.scalatest.{Matchers, WordSpec}
import bifrost.block.Block
import bifrost.blocks.BifrostBlock
import bifrost.transaction.BifrostTransaction
import bifrost.transaction.box.ArbitBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519

import scala.concurrent.Await
import scala.concurrent.duration._


class UtilsRPCSpec extends WordSpec
  with Matchers
  with ScalatestRouteTest
  with BifrostGenerators {

  val route = UtilsApiRouteRPC(settings).route

  def httpPOST(jsonRequest: ByteString): HttpRequest = {
    HttpRequest(
      HttpMethods.POST,
      uri = "/utilsRPC/",
      entity = HttpEntity(MediaTypes.`application/json`, jsonRequest)
    )
  }


}

