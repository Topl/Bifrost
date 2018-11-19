package bifrost.contract.modules

import java.io.{InputStream, InputStreamReader}
import java.nio.file.{Files, Path}

import javax.script.{Bindings, ScriptContext}
import akka.actor.ActorSystem

import collection.JavaConverters._
import akka.http.scaladsl.coding.Gzip
import akka.stream.ActorMaterializer
import akka.util.ByteString
import bifrost.contract.{Contract, ValkyrieFunctions}
import io.circe._
import io.circe.syntax._
import io.circe.parser._
import jdk.nashorn.api.scripting.{JSObject, NashornScriptEngine, NashornScriptEngineFactory, ScriptObjectMirror}
import scorex.core.serialization.JsonSerializable
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.proof.Signature25519
import scorex.crypto.encode.{Base58, Base64}

import scala.collection.mutable
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

/**
  * Created by Matt Kindy on 7/27/2017.
  */
case class BaseModuleWrapper(name: String,
                             initjs: String,
                             registry: Map[String, mutable.LinkedHashSet[String]],
                             state: Json,
                             signed: Option[(PublicKey25519Proposition, Signature25519)]) extends JsonSerializable {

  lazy val json: Json = Map(
    "state" -> Base64.encode(Gzip.encode(ByteString(state.noSpaces.getBytes)).toArray[Byte]).asJson,
    "name" -> name.asJson,
    "initjs" -> Base64.encode(Gzip.encode(ByteString(initjs.getBytes)).toArray[Byte]).asJson,
    "registry" -> registry.map(a => a._1 -> a._2.map(_.asJson).asJson).asJson,
    "signed" -> signed.map(pair => Base58.encode(pair._1.pubKeyBytes) -> Base58.encode(pair._2.bytes)).asJson
  ).asJson
}

object BaseModuleWrapper {

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

  def apply(modulePath: Path)(args: JsonObject): BaseModuleWrapper = {

    /* Read file from path, expect JSON */
    val parsed = parse(new String(Files.readAllBytes(modulePath)))

    parsed match {
      case Left(f) => throw f
      case Right(json) => wrapperFromJson(json, args)
    }
  }

  def apply(name: String, initjs: String, signed: Option[(PublicKey25519Proposition, Signature25519)] = None)(args: JsonObject): BaseModuleWrapper = {

    val modifiedInitjs = initjs.replaceFirst("\\{", "\\{\n" + ValkyrieFunctions.reserved + "\n")
    println(">>>>>>>>>>>>>>>>>>>>> initjs + reservedFunctions: " + modifiedInitjs)

    val (registry, cleanModuleState) = deriveFromInit(modifiedInitjs, name)(args)

    BaseModuleWrapper(name, modifiedInitjs, registry, parse(cleanModuleState).right.getOrElse(JsonObject.empty.asJson), signed)
  }

  private def wrapperFromJson(json: Json, args: JsonObject): BaseModuleWrapper = {
    /* Expect name to be top level */
    val name: String = (json \\ "module_name").head.asString.get

    /* Expect initjs to be top level and load it up */

    val initjs: String = {
      val cleanInitjs: String = (json \\ "initjs").head.asString.get
      val modifiedInitjs = cleanInitjs.replaceFirst("\\{", "\\{\n" + ValkyrieFunctions.reserved + "\n")
      println(">>>>>>>>>>>>>>>>>>>>> initjs + reservedFunctions: " + modifiedInitjs)
      modifiedInitjs
    }

    val announcedRegistry: Option[Map[String, mutable.LinkedHashSet[String]]] =
      (json \\ "registry").headOption.map(_.as[Map[String, mutable.LinkedHashSet[String]]].right.get)

    val signed: Option[(PublicKey25519Proposition, Signature25519)] = (json \\ "signed")
      .headOption
      .map(_.as[(String, String)].right.get)
      .map(pair => PublicKey25519Proposition(Base58.decode(pair._1).get) -> Signature25519(Base58.decode(pair._2).get))

    val (registry, cleanModuleState) = deriveFromInit(initjs, name, announcedRegistry)(args)

    BaseModuleWrapper(name, initjs, registry, parse(cleanModuleState).right.get, signed)
  }

  private def deriveFromInit(initjs: String, name: String, announcedRegistry: Option[Map[String, mutable.LinkedHashSet[String]]] = None)(args: JsonObject):
    (Map[String, mutable.LinkedHashSet[String]], String) = {

    /* Construct base module from params */
    val jsre: NashornScriptEngine = new NashornScriptEngineFactory().getScriptEngine.asInstanceOf[NashornScriptEngine]

    jsre.eval(objectAssignPolyfill)
    jsre.eval(initjs)
    jsre.eval(s"var c = $name.fromJSON('${args.asJson.noSpaces}')")
    val cleanModuleState: String = jsre.eval(s"$name.toJSON(c)").asInstanceOf[String]

    /* Interpret registry from object */
    val esprimajs: InputStream = classOf[BaseModuleWrapper].getResourceAsStream("/esprima.js")
    jsre.eval(new InputStreamReader(esprimajs))

    val defineEsprimaFnParamParser =
      s"""
        |function getParameters(f) {
        |    var parsed = esprima.parse("safetyValve = " + f.toString().replace("[native code]", ""));
        |    var params = parsed.body[0].expression.right.params;
        |    var ret = [];
        |    params.forEach(function(p){ ret.push(p.name); })
        |    return ret;
        |}
      """.stripMargin

    jsre.eval(defineEsprimaFnParamParser)

    val registry = if(announcedRegistry.isDefined && checkRegistry(jsre, announcedRegistry.get)) {
      announcedRegistry.get
    } else {
      val registryRes = deriveRegistry(jsre)
      registryRes.entrySet().asScala.map(entry => entry.getKey -> mutable.LinkedHashSet(entry.getValue.asInstanceOf[Array[String]]:_*)).toMap
    }

    println(s">>>>>>>>>>> Registry: $registry")
    (registry, cleanModuleState)
  }

  private def checkRegistry(jsre: NashornScriptEngine, announcedRegistry: Map[String, mutable.LinkedHashSet[String]]): Boolean = {
    announcedRegistry.keySet.forall(k => {
      jsre.eval(
        s"""
           |typeof c.$k === "function" ? getParameters(c.$k).length === ${announcedRegistry(k).size} : false
         """.stripMargin
      ).asInstanceOf[Boolean]
    })
  }

  private def deriveRegistry(jsre: NashornScriptEngine): ScriptObjectMirror = {
    val getProperties =
      s"""
         |var classFunctions = Object.getOwnPropertyNames(c)
         |var protoFunctions = Object.getOwnPropertyNames(Object.getPrototypeOf(c))
         |var strArrType = Java.type("java.lang.String[]")
         |
         |classFunctions.concat(protoFunctions).reduce(function(a, fnName) {
         |  if(typeof c[fnName] === "function") {
         |    a[fnName] = Java.to(getParameters(c[fnName]), strArrType);
         |  }
         |  return a;
         |}, {})
       """.
        stripMargin

    jsre.eval(getProperties).asInstanceOf[ScriptObjectMirror]
  }

  implicit val system = ActorSystem("QuickStart")
  implicit val materializer = ActorMaterializer()

  implicit val encodeTerms: Encoder[BaseModuleWrapper] = (b: BaseModuleWrapper) => b.json

  implicit val decodeTerms: Decoder[BaseModuleWrapper] = (c: HCursor) => for {
    state <- c.downField("state").as[String]
    name <- c.downField("name").as[String]
    initjs <- c.downField("initjs").as[String]
    registry <- c.downField("registry").as[Map[String, mutable.LinkedHashSet[String]]]
    signed <- c.downField("signed").as[Option[(String, String)]]
  } yield {

    def decodeGzip(zipped: String): Future[ByteString] = {
      Gzip.decode(ByteString(Base64.decode(zipped)))
    }

    Await.result({
      import scala.concurrent.ExecutionContext.Implicits.global
      for {
        decodedInitjs <- decodeGzip(initjs)
        decodedState <- decodeGzip(state)
      } yield BaseModuleWrapper(
        name,
        new String(decodedInitjs.toArray[Byte]),
        registry,
        parse(new String(decodedState.toArray[Byte])).right.get,
        signed.map(pair => PublicKey25519Proposition(Base58.decode(pair._1).get) -> Signature25519(Base58.decode(pair._2).get))
      )
    }, Duration.Inf)
  }
}
