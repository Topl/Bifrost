package bifrost.http.api.routes

import akka.actor.{ActorRef, ActorRefFactory}
import akka.http.scaladsl.server.Route
import bifrost.crypto.PrivateKey25519Companion
import bifrost.exceptions.JsonParsingException
import bifrost.history.History
import bifrost.http.api.ApiRouteWithView
import bifrost.mempool.MemPool
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.{Box, CodeBox, ExecutionBox, StateBox}
import bifrost.modifier.transaction.bifrostTransaction.{CodeCreation, ProgramCreation, ProgramMethodExecution, ProgramTransfer}
import bifrost.nodeView.GenericNodeViewHolder.ReceivableMessages.LocallyGeneratedTransaction
import bifrost.program.{ExecutionBuilder, ExecutionBuilderTerms, ProgramPreprocessor}
import bifrost.settings.AppSettings
import bifrost.state.State
import bifrost.wallet.Wallet
import io.circe.literal._
import io.circe.syntax._
import io.circe.{Decoder, Json, JsonObject}
import scorex.crypto.encode.Base58

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

case class ProgramApiRoute(override val settings: AppSettings, nodeViewHolderRef: ActorRef)
                          (implicit val context: ActorRefFactory) extends ApiRouteWithView {
  type HIS = History
  type MS = State
  type VL = Wallet
  type MP = MemPool
  override val route: Route = pathPrefix("program") { basicRoute(handlers) }

  def handlers(method: String, params: Vector[Json], id: String): Future[Json] =
    method match {
      case "getProgramSignature" => getProgramSignature(params.head, id)
      case "createCode" => createCode(params.head, id)
      case "createProgram" => createProgram(params.head, id)
      case "transferProgram" => transferProgram(params.head, id)
      case "executeProgramMethod" => executeProgramMethod(params.head, id)
      case "programCall" => programCall(params.head, id)
      case "filter" => bloomFilter(params, id)
    }

  def getProgramSignature(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val signingPublicKey = (params \\ "signingPublicKey").head.asString.get
      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val state = view.state
      val tx = createProgramInstance(params, state)
      val signature = PrivateKey25519Companion.sign(selectedSecret, tx.messageToSign)
      Map("signature" -> Base58.encode(signature.signature).asJson,
        "tx" -> tx.json.asJson).asJson
    }
  }

  def createCode(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val owner = PublicKey25519Proposition(Base58.decode((params \\ "publicKey").head.asString.get).get)
      val code: String = (params \\ "code").head.asString.get
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      val tx = CodeCreation.createAndApply(wallet, owner, code, fee, data).get

      CodeCreation.validate(tx) match {
        case Success(_) =>
          nodeViewHolderRef ! LocallyGeneratedTransaction[CodeCreation](tx)
          tx.json
        case Failure(e) => throw new Exception(s"Could not validate transaction: $e")
      }
    }
  }

  def createProgram(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val state = view.state
      val tx = createProgramInstance(params, state)
//      ProgramCreation.validate(tx) match {
//        case Success(_) => log.info("Program creation validated successfully")
//        case Failure(e) => throw e
//      }
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramCreation](tx)
      tx.json
    }
  }

  def transferProgram(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val tbr = view.state.tbr
      val from = PublicKey25519Proposition(Base58.decode((params \\ "from").head.asString.get).get)
      val to = PublicKey25519Proposition(Base58.decode((params \\ "to").head.asString.get).get)
      val executionBox = tbr.closedBox(Base58.decode((params \\ "programId").head.asString.get).get).get.asInstanceOf[ExecutionBox]
      val fee: Long = (params \\ "fee").head.asNumber.flatMap(_.toLong).getOrElse(0L)
      val data: String = (params \\ "data").headOption match {
        case Some(dataStr) => dataStr.asString.getOrElse("")
        case None => ""
      }

      val tx = ProgramTransfer.createAndApply(wallet, from, to, executionBox, fee, data).get

      nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramTransfer](tx)
      tx.json
    }
  }

  def executeProgramMethod(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val wallet = view.vault
      val signingPublicKey = (params \\ "owner").head.asString.get

      //TODO Replace with method to return all boxes from program box registry
      val executionBox = programBoxId2Box(view.state, (params \\ "programId").head.asString.get).asInstanceOf[ExecutionBox]
      val state: Seq[StateBox] = executionBox.stateBoxUUIDs.map { sb =>
        view.state.pbr.getBox(sb).get.asInstanceOf[StateBox]
      }
      val code: Seq[CodeBox] = executionBox.codeBoxIds.map { cb =>
        //programBoxId2Box(view.state, Base58.encode(cb)).asInstanceOf[CodeBox]
        view.state.getBox(cb).get.asInstanceOf[CodeBox]
      }

      val programJson: Json =
        json"""{
            "executionBox": ${executionBox.json},
            "state": ${state.map(sb => sb.json).asJson},
            "code": ${code.map(cb => cb.json).asJson}
            }"""

      val modifiedParams = params.hcursor.downField("programId").delete.top.get.deepMerge(programJson)

      val selectedSecret = wallet.secretByPublicImage(PublicKey25519Proposition(Base58.decode(signingPublicKey).get)).get
      val tempTx = modifiedParams.as[ProgramMethodExecution] match {
        case Right(p: ProgramMethodExecution) => p
        case Left(e) => throw new JsonParsingException(s"Could not parse ProgramMethodExecution: $e")
      }
      val realSignature = PrivateKey25519Companion.sign(selectedSecret, tempTx.messageToSign)
      val tx = tempTx.copy(signatures = Map(PublicKey25519Proposition(Base58.decode(signingPublicKey).get) -> realSignature))
//      ProgramMethodExecution.validate(tx) match {
//        case Success(_) => log.info("Program method execution successfully validated")
//        case Failure(e) => throw e.getCause
//      }

      tx.newBoxes.toSet
      nodeViewHolderRef ! LocallyGeneratedTransaction[ProgramMethodExecution](tx)
      tx.json
    }
  }

  def programCall(params: Json, id: String): Future[Json] = {
    viewAsync().map { view =>
      val pbr = view.state.pbr
      val tbr = view.state.tbr
      val programId = (params \\ "programId").head.asString.get
      val stateVar = (params \\ "stateVar").head.asString.get
      val program: ExecutionBox = tbr.closedBox(Base58.decode(programId).get).get.asInstanceOf[ExecutionBox]
      val programState: StateBox = pbr.getBox(program.stateBoxUUIDs.head).get.asInstanceOf[StateBox]
      val result: Decoder.Result[Json] = programState.state.hcursor.downField(stateVar).as[Json]

      result match {
        case Right(value) => value
        case Left(error) => error.getMessage.asJson
      }
    }
  }

  def bloomFilter(params: Vector[Json], id: String): Future[Json] = {
    viewAsync().map { view =>
      val history = view.history
      val queryBloomTopics = params.map(j => j.asString.getOrElse("")).map(s => Base58.decode(s).get)
      val res = history.bloomFilter(queryBloomTopics)
      res.map(_.json).asJson
    }
  }

  //TODO Return ProgramBox instead of Box
  private def programBoxId2Box(state: State, boxId: String): Box = {
    state.getBox(Base58.decode(boxId).get).get
  }

  //noinspection ScalaStyle
  def createProgramInstance(json: Json, state: State): ProgramCreation = {
    val program = (json \\ "program").head.asString.get
    val preProcess = ProgramPreprocessor("program", program)(JsonObject.empty)
    val builder = Map("executionBuilder" -> ExecutionBuilder(ExecutionBuilderTerms(""), "", preProcess).json).asJson

    val strippedProgram: Json = json.hcursor.downField("program").delete.top.get

    val preparedProgram: Json = strippedProgram.deepMerge(builder)

    preparedProgram.as[ProgramCreation] match {
      case Right(c: ProgramCreation) => c
      case Left(e) => throw new JsonParsingException(s"Could not parse ProgramCreation: $e")
    }
  }
}