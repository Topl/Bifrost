package bifrost.program

import java.nio.file.{Files, Path}

import akka.actor.ActorSystem
import akka.http.scaladsl.coding.Gzip
import akka.stream.ActorMaterializer
import akka.util.ByteString
import io.circe._
import io.circe.parser._
import io.circe.syntax._
import bifrost.serialization.JsonSerializable
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.transaction.proof.Signature25519
import com.oracle.js.parser.ir.visitor.NodeVisitor
import com.oracle.js.parser.ir.{FunctionNode, LexicalContext, Node, VarNode}
import com.oracle.js.parser.{ErrorManager, Lexer, Parser, ScriptEnvironment, Source, Token, TokenStream, TokenType}
import org.graalvm.polyglot.{Context, Value}
import scorex.crypto.encode.{Base58, Base64}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex

/**
  * Created by Matt Kindy on 7/27/2017.
  */
case class ProgramPreprocessor(name: String,
                               initjs: String,
                               interface: Map[String, Seq[String]],
                               //state: Json,
                               variables: Json,
                               code: Map[String, String],
                               signed: Option[(PublicKey25519Proposition, Signature25519)]) extends JsonSerializable {

  lazy val json: Json = Map(
    //"state" -> Base64.encode(Gzip.encode(ByteString(state.noSpaces.getBytes)).toArray[Byte]).asJson,
    "name" -> name.asJson,
    "initjs" -> Base64.encode(Gzip.encode(ByteString(initjs.getBytes)).toArray[Byte]).asJson,
    "interface" -> interface.map(a => a._1 -> a._2.map(_.asJson).asJson).asJson,
    "variables" -> variables.asJson,
    "code" -> code.map(a => a._1 -> a._2).asJson,
    "signed" -> signed.map(pair => Base58.encode(pair._1.pubKeyBytes) -> Base58.encode(pair._2.bytes)).asJson
  ).asJson
}

object ProgramPreprocessor {

  val objectAssignPolyfill: String =
    s"""
       |if (typeof Object.assign != 'function') {
       |  // Must be writable: true, enumerable: false, configurable: true
       |  Object.defineProperty(Object, "assign", {
       |    value: function assign(target, varArgs) { // .length of function is 2
       |      'use strict';
       |      if (target == null) { // TypeError if undefined or null
       |        throw new TypeError('Cannot convert undefined or null to object');
       |      }
       |
       |      var to = Object(target);
       |
       |      for (var index = 1; index < arguments.length; index++) {
       |        var nextSource = arguments[index];
       |
       |        if (nextSource != null) { // Skip over if undefined or null
       |          for (var nextKey in nextSource) {
       |            // Avoid bugs when hasOwnProperty is shadowed
       |            if (Object.prototype.hasOwnProperty.call(nextSource, nextKey)) {
       |              to[nextKey] = nextSource[nextKey];
       |            }
       |          }
       |        }
       |      }
       |      return to;
       |    },
       |    writable: true,
       |    configurable: true
       |  });
       |}
     """.stripMargin

  /* TODO sanitise inputs!! */

  def apply(modulePath: Path)(args: JsonObject): ProgramPreprocessor = {

    /* Read file from path, expect JSON */
    val parsed = parse(new String(Files.readAllBytes(modulePath)))

    parsed match {
      case Left(f) => throw f
      case Right(json) => wrapperFromJson(json, args)
    }
  }

  def apply(name: String, initjs: String, signed: Option[(PublicKey25519Proposition, Signature25519)] = None)(args: JsonObject): ProgramPreprocessor = {

    //val modifiedInitjs = initjs.replaceFirst("\\{", "\\{\n" + ValkyrieFunctions().reserved + "\n")
    //println(">>>>>>>>>>>>>>>>>>>>> initjs + reservedFunctions: " + modifiedInitjs)

    val (interface, /*cleanModuleState,*/ variables, code) = deriveFromInit(initjs /*modifiedInitjs*/, name)(args)

    ProgramPreprocessor(name, initjs /*modifiedInitjs*/, interface, /*parse(cleanModuleState).right.getOrElse(JsonObject.empty.asJson),*/ variables, code, signed)
  }

  private def wrapperFromJson(json: Json, args: JsonObject): ProgramPreprocessor = {
    /* Expect name to be top level */
    val name: String = (json \\ "module_name").head.asString.get

    /* Expect initjs to be top level and load it up */

    val initjs: String = {
      val cleanInitjs: String = (json \\ "initjs").head.asString.get
      //val modifiedInitjs = cleanInitjs.replaceFirst("\\{", "\\{\n" + ValkyrieFunctions().reserved + "\n")
      //println(">>>>>>>>>>>>>>>>>>>>> initjs + reservedFunctions: " + modifiedInitjs)
      //modifiedInitjs
      cleanInitjs
    }

    val announcedRegistry: Option[Map[String, Seq[String]]] =
      (json \\ "interface").headOption.map(_.as[Map[String, Seq[String]]].right.get)

    val signed: Option[(PublicKey25519Proposition, Signature25519)] = (json \\ "signed")
      .headOption
      .map(_.as[(String, String)].right.get)
      .map(pair => PublicKey25519Proposition(Base58.decode(pair._1).get) -> Signature25519(Base58.decode(pair._2).get))

    val (interface, /*cleanModuleState,*/ variables, code) = deriveFromInit(initjs, name, announcedRegistry)(args)

    ProgramPreprocessor(name, initjs, interface, /*parse(cleanModuleState).right.get,*/ variables, code, signed)
  }

  //noinspection ScalaStyle
  private def deriveFromInit(initjs: String, name: String, announcedRegistry: Option[Map[String, Seq[String]]] = None)(args: JsonObject):
    (Map[String, Seq[String]], /*String,*/ Json, Map[String, String]) = {

    val jsre: Context = Context.create("js")

    /*jsre.eval("js", objectAssignPolyfill)
    jsre.eval("js", initjs)
    jsre.eval("js", s"var c = $name.fromJSON('${args.asJson.noSpaces}')")
    println(s">>>>>>>> var c: ")
    jsre.eval("js", "for(property in c) { print(property) }")
    val cleanModuleState: Value = jsre.eval("js", s"$name.toJSON(c)") //.asInstanceOf[String]
    val cms = cleanModuleState.getSourceLocation
    println(cms)
     */

    /* Interpret interface from object */
   /* val esprimajs: InputStream = classOf[ProgramPreprocessor].getResourceAsStream("/esprima.js")
    jsre.eval(new InputStreamReader(esprimajs))*/

    /*val defineEsprimaFnParamParser =
      s"""
        |function getParameters(f) {
        |    var parsed = esprima.parse("safetyValve = " + f.toString().replace("[native code]", ""));
        |    var params = parsed.body[0].expression.right.params;
        |    var ret = [];
        |    params.forEach(function(p){ ret.push(p.name); })
        |    return ret;
        |}
      """.stripMargin

    jsre.eval(defineEsprimaFnParamParser)*/


    //println(s">>>>>>>>>>> Registry: $interface")

    //val variables: Seq[String] = deriveState(jsre, initjs)
    //val code: Seq[String] = deriveFunctions(jsre, initjs)

    val variables: Json = deriveState(jsre, initjs)

    val code: Map[String, String] = deriveFunctions(jsre, initjs)

    val interface = if(announcedRegistry.isDefined && checkRegistry(jsre, announcedRegistry.get)) {
      announcedRegistry.get
    } else {
      val interfaceRes = deriveRegistry(jsre, initjs)

      code.keySet.zip(interfaceRes).toMap
    }

    (interface, /*cleanModuleState,*/ variables, code)
  }

  private def checkRegistry(jsre: Context, announcedRegistry: Map[String, Seq[String]]): Boolean = {
    announcedRegistry.keySet.forall(k => {
      jsre.eval("js",
        s"""
           |typeof c.$k === "function" ? getParameters(c.$k).length === ${announcedRegistry(k).size} : false
         """.stripMargin
      ).asInstanceOf[Boolean]
    })
  }

  private def deriveRegistry(jsre: Context, initjs: String): Seq[Seq[String]] = {

    def commentTokenSource(source: Source): Seq[String] = {

      var commentList: ListBuffer[String] = new ListBuffer[String]()

      val tokenStream: TokenStream = new TokenStream

      val lexer = new Lexer(source, tokenStream, false, 8, false, false, true)

      lexer.lexify()

      for (i <- 0 until tokenStream.last()) {
        val token = tokenStream.get(i)
        if(Token.descType(tokenStream.get(i)) == TokenType.COMMENT &&
          Token.descType(tokenStream.get(i + 4)) == TokenType.FUNCTION) {
          commentList += source.getString(token)
        }
      }

      commentList
    }

    def paramTypes(commentString: Seq[String]): Seq[Seq[String]] = {

      val pattern: Regex = """(?<=@param \{)[A-Za-z]+(?=\})""".r

      commentString.map { str =>
        pattern.findAllIn(str).toSeq
      }
    }

    paramTypes(commentTokenSource(Source.sourceFor("initjs", initjs)))
  }

  //noinspection ScalaStyle
  private def deriveState(jsre: Context, initjs: String): Json = {
    //val initjsStr = s"\'${initjs.replaceAll("\n", "\\\\n").trim}\'"

    val scriptEnv: ScriptEnvironment = ScriptEnvironment.builder
      .ecmaScriptVersion(8)
      .constAsVar(false)
      .earlyLvalueError(true)
      .emptyStatements(false)
      .syntaxExtensions(true)
      .scripting(false)
      .shebang(false)
      .strict(true)
      .functionStatementBehavior(ScriptEnvironment.FunctionStatementBehavior.ERROR)
      .build()


    val errManager = new ErrorManager.ThrowErrorManager
    val src = Source.sourceFor("script", initjs)
    val parser: Parser = new Parser(scriptEnv, src, errManager)
    val parsed = parser.parse()

    def varList(node: FunctionNode): Json = {

      var vars = scala.collection.mutable.Map[String, String]()
      var varJson = scala.collection.mutable.Map[String, Json]()

      node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

        override def leaveVarNode(varNode: VarNode): VarNode = {
          val name: String = varNode.getName.getName
          val init = varNode.getInit.toString
          vars += (name -> init)
          varNode
        }

      })

      jsre.eval("js", initjs)

      vars.map{ v =>
        jsre.eval("js", s"typeof ${v._1}").toString match {
          case "number" => varJson += (v._1 -> JsonNumber.fromString(v._2.toString).get.asJson)
          case "string" => varJson += (v._1 -> v._2.asJson)
          case _ => throw new ClassCastException("Not a valid JavaScript type")
        }
      }
      varJson.toMap.asJson
    }

    varList(parsed)
  }

  private def deriveFunctions(jsre: Context, initjs: String): Map[String, String] = {


    val scriptEnv: ScriptEnvironment = ScriptEnvironment.builder
      .ecmaScriptVersion(8)
      .constAsVar(false)
      .earlyLvalueError(true)
      .emptyStatements(false)
      .syntaxExtensions(true)
      .scripting(false)
      .shebang(false)
      .strict(true)
      .functionStatementBehavior(ScriptEnvironment.FunctionStatementBehavior.ERROR)
      .build()

    val errManager = new ErrorManager.ThrowErrorManager
    val src = Source.sourceFor("script", initjs)
    val parser: Parser = new Parser(scriptEnv, src, errManager)
    val parsed = parser.parse()

    def functionList(node: FunctionNode): Map[String, String] = {

      var functions = mutable.LinkedHashMap[String, String]()

      node.getBody.accept(new NodeVisitor[LexicalContext](new LexicalContext) {

        override def leaveFunctionNode(functionNode: FunctionNode): Node = {
          functions += functionNode.getName -> s"$functionNode = {${functionNode.getBody}}"
          functionNode
        }
      })
      functions.toMap
    }

    functionList(parsed)
  }

  implicit val system = ActorSystem("QuickStart")
  implicit val materializer: ActorMaterializer = ActorMaterializer()

  implicit val encodeTerms: Encoder[ProgramPreprocessor] = (b: ProgramPreprocessor) => b.json

  implicit val decodeTerms: Decoder[ProgramPreprocessor] = (c: HCursor) => for {
    //state <- c.downField("state").as[String]
    name <- c.downField("name").as[String]
    initjs <- c.downField("initjs").as[String]
    interface <- c.downField("interface").as[Map[String, Seq[String]]]
    variables <- c.downField("variables").as[Json]
    code <- c.downField("code").as[Map[String, String]]
    signed <- c.downField("signed").as[Option[(String, String)]]
  } yield {

    def decodeGzip(zipped: String): Future[ByteString] = {
      Gzip.decode(ByteString(Base64.decode(zipped)))
    }

    Await.result({
      import scala.concurrent.ExecutionContext.Implicits.global
      for {
        decodedInitjs <- decodeGzip(initjs)
        //decodedState <- decodeGzip(state)
      } yield ProgramPreprocessor(
        name,
        new String(decodedInitjs.toArray[Byte]),
        interface,
        //parse(new String(decodedState.toArray[Byte])).right.get,
        variables,
        code,
        signed.map(pair => PublicKey25519Proposition(Base58.decode(pair._1).get) -> Signature25519(Base58.decode(pair._2).get))
      )
    }, Duration.Inf)
  }
}
