//accumulators co.topl.http.api.routes
//
//import akka.actor.{ActorRef, ActorRefFactory}
//import akka.http.scaladsl.server.Route
//import co.topl.attestation.AddressEncoder.NetworkPrefix
//import co.topl.http.api.ApiRouteWithView
//import co.topl.modifier.transaction.ProgramCreation
//import co.topl.nodeView.NodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
//import co.topl.nodeView.history.History
//import co.topl.nodeView.mempool.MemPool
//import co.topl.modifier.box.{ExecutionBox, StateBox}
//import co.topl.nodeView.state.State
//import co.topl.program.{ExecutionBuilder, ExecutionBuilderTerms, ProgramPreprocessor}
//import co.topl.settings.{AppContext, RPCApiSettings}
//import co.topl.utils.exceptions.JsonParsingException
//import io.circe.literal._
//import io.circe.syntax._
//import io.circe.{Decoder, Json, JsonObject}
//import co.topl.crypto.utils.Base58
//
//import scala.concurrent.ExecutionContext.Implicits.global
//import scala.concurrent.Future
//import scala.util.{Failure, Success}
//
//case class ProgramApiRoute(override val settings: RPCApiSettings, appContext: AppContext, nodeViewHolderRef: ActorRef)
//                          (implicit val context: ActorRefFactory) extends ApiRouteWithView {
//  type HIS = History
//  type MS = State
//  type MP = MemPool
//  override val route: Route = { basicRoute(handlers) }
//
//  // Establish the expected network prefix for addresses
//  implicit val networkPrefix: NetworkPrefix = appContext.networkType.netPrefix
//
//  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
//    method match {
////      case "getProgramSignature" => getProgramSignature(params.head, id)
////      case "createCode" => createCode(params.head, id)
//      case "createProgram" => createProgram(params.head, id)
////      case "transferProgram" => transferProgram(params.head, id)
////      case "executeProgramMethod" => executeProgramMethod(params.head, id)
//      case "programCall" => programCall(params.head, id)
//      case "filter" => bloomFilter(params, id)
//    }
//
////  def getProgramSignature(params: Json, id: String): Future[Json] = {
////    viewAsync().map { view =>
////      val wallet = view.vault
////      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
////      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(PublicKey(Base58.decode(signingPublicKey).get))).get
////      val state = view.state
////      val tx = createProgramInstance(params, state)
////      val signature = selectedSecret.sign(tx.messageToSign)
////      Map("signature" -> Base58.encode(signature.signature).asJson,
////        "tx" -> tx.json.asJson).asJson
////    }
////  }
//
////  def createCode(params: Json, id: String): Future[Json] = {
////    viewAsync().map { view =>
////      val wallet = view.vault
////      val owner = PublicKey25519Proposition((params \\ "publicKey").head.asString.get).get
////      val code: String = (params \\ "code").head.asString.get
////      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
////      val data: String = (params \\ "data").headOption match {
////        case Some(dataStr) => dataStr.asString.getOrElse("")
////        case None => ""
////      }
////
////      val tx = CodeCreation.createAndApply(wallet, owner, code, fee, data).get
////
////      CodeCreation.semanticValidate(tx, view.state) match {
////        case Success(_) =>
////          nodeViewHolderRef ! LocallyGeneratedTransaction[CodeCreation](tx)
////          tx.json
////        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
////      }
////    }
////  }
//
//  def createProgram(params: Json, id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val state = view.state
//      val tx = createProgramInstance(params, state)
//
//      ProgramCreation.semanticValidate(tx, view.state) match {
//        case Success(_) =>
//          nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramCreation](tx)
//          tx.json
//        case Failure(e) => throw new Error("Failed to validate the program creation")
//      }
//    }
//  }
//
////  def transferProgram(params: Json, id: String): Future[Json] = {
////    viewAsync().map { view =>
////      val wallet = view.vault
////      val from = PublicKey25519Proposition((params \\ "from").head.asString.get).get
////      val to = PublicKey25519Proposition((params \\ "to").head.asString.get).get
////      val executionBox = view.state.getProgramBox[ExecutionBox](ProgramId((params \\ "programId").head.asString.get).get).get
////      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
////
////      val data: String = (params \\ "data").headOption match {
////        case Some(dataStr) => dataStr.asString.getOrElse("")
////        case None => ""
////      }
////
////      val tx = ProgramTransfer.createAndApply(wallet, from, to, executionBox, fee, data).get
////
////      nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramTransfer](tx)
////      tx.json
////    }
////  }
//
////  def executeProgramMethod(params: Json, id: String): Future[Json] = {
////    viewAsync().map { view =>
////      val wallet = view.vault
////      val signingPublicKey = (params \\ "owner").head.asString.get
////
////      val executionBox = view.state.getProgramBox[ExecutionBox](ProgramId((params \\ "programId").head.asString.get).get).get
////
////      val state = executionBox.stateBoxIds.map { sb =>
////        view.state.getProgramBox[StateBox](sb).get
////      }
////
////      val code = executionBox.codeBoxIds.map { cb =>
////        view.state.getProgramBox[CodeBox](cb).get
////      }
////
////      val programJson: Json =
////        json"""{
////            "executionBox": ${executionBox.json},
////            "state": ${state.map(sb => sb.json).asJson},
////            "code": ${code.map(cb => cb.json).asJson}
////            }"""
////
////      val modifiedParams = params.hcursor.downField("programId").delete.top.get.deepMerge(programJson)
////
////      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(PublicKey(Base58.decode(signingPublicKey).get))).get
////      val tempTx = modifiedParams.as[ProgramMethodExecution] match {
////        case Right(p: ProgramMethodExecution) => p
////        case Left(e) => throw new JsonParsingException(s"Could not parse ProgramMethodExecution: $e")
////      }
////      val realSignature = selectedSecret.sign(tempTx.messageToSign)
////      val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(PublicKey(Base58.decode(signingPublicKey).get)) -> realSignature))
////
////      ProgramMethodExecution.semanticValidate(tx, view.state) match {
////        case Success(_) =>
////          nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramMethodExecution](tx)
////          tx.json
////
////        case Failure(e) => throw e.getCause
////      }
////    }
////  }
//
//  def programCall(params: Json, id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val programId = ProgramId((params \\ "programId").head.asString.get)
//      val stateVar = (params \\ "stateVar").head.asString.get
//
//      val program = view.state.getProgramBox[ExecutionBox](programId).get
//
//      val programState = view.state.getProgramBox[StateBox](program.stateBoxIds.head).get
//
//      val result: Decoder.Result[Json] =
//        programState
//          .state
//          .hcursor
//          .downField(stateVar)
//          .as[Json]
//
//      result match {
//        case Right(value) => value
//        case Left(error) => error.getMessage.asJson
//      }
//    }
//  }
//
//  def bloomFilter(params: Vector[Json], id: String): Future[Json] = {
//    viewAsync().map { view =>
//      val history = view.history
//      val queryBloomTopics = params.map(j => j.asString.getOrElse("")).map(s => Base58.decode(s).get)
//      val res = history.bloomFilter(queryBloomTopics)
//      res.map(_.json).asJson
//    }
//  }
//
//  //noinspection ScalaStyle
//  def createProgramInstance(json: Json, state: State): ProgramCreation = {
//    val program = (json \\ "program").head.asString.get
//    val preProcess = ProgramPreprocessor("program", program)(JsonObject.empty)
//    val builder = Map("executionBuilder" -> ExecutionBuilder(ExecutionBuilderTerms(""), "", preProcess).json).asJson
//
//    val strippedProgram: Json = json.hcursor.downField("program").delete.top.get
//
//    val preparedProgram: Json = strippedProgram.deepMerge(builder)
//
//    preparedProgram.as[ProgramCreation] match {
//      case Right(c: ProgramCreation) => c
//      case Left(e) => throw new JsonParsingException(s"Could not parse ProgramCreation: $e")
//    }
//  }
//}
