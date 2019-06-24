package bifrost.program

import akka.actor.{ActorRef, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.http.scaladsl.server.Route
import akka.pattern.ask
import akka.stream.ActorMaterializer
import akka.util.{ByteString, Timeout}
import bifrost.{BifrostLocalInterface, BifrostNodeViewHolder}
import bifrost.api.http.{AssetApiRoute, WalletApiRoute}
import bifrost.forging.ForgingSettings
import bifrost.history.BifrostHistory
import bifrost.mempool.BifrostMemPool
import bifrost.scorexMod.GenericNodeViewHolder.{CurrentView, GetCurrentView}
import bifrost.state.BifrostState
import bifrost.wallet.BWallet
import com.oracle.truffle.api.CompilerDirectives
import com.oracle.truffle.api.frame.VirtualFrame
import com.oracle.truffle.api.instrumentation.StandardTags.CallTag
import bifrost.program.ValkyrieController
import com.oracle.truffle.api.instrumentation._
import io.circe
import io.circe.JsonObject
import org.graalvm.polyglot.Context

import scala.concurrent.{Await, ExecutionContextExecutor, Future}
import scala.concurrent.duration._
import scala.util.{Failure, Success}
import scala.language.existentials

abstract class ValkyrieController {

}
