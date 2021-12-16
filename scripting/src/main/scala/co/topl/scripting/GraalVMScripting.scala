package co.topl.scripting

import cats.effect._
import cats.implicits._
import io.circe.syntax._
import io.circe.{Json, JsonNumber, JsonObject}
import org.graalvm.polyglot.proxy.{ProxyArray, ProxyObject}
import org.graalvm.polyglot.{Context, HostAccess, PolyglotAccess, Value}

import java.util
import scala.language.implicitConversions

object GraalVMScripting {

  private val ctx = Context
    .newBuilder()
    .allowExperimentalOptions(true)
    .option("js.experimental-foreign-object-prototype", "true")
    .allowPolyglotAccess(PolyglotAccess.ALL)
    .allowHostAccess(
      HostAccess.newBuilder().allowPublicAccess(true).allowArrayAccess(true).allowListAccess(true).build()
    )
    .build()

  def jsExecutor[F[_]: Sync, Res: GraalVMValuable](script: String): F[Seq[Value] => F[Res]] =
    languageExecutor[F, Res]("js", script)

  def pythonExecutor[F[_]: Sync, Res: GraalVMValuable](script: String): F[Seq[Value] => F[Res]] =
    languageExecutor[F, Res]("python", script)

  private def languageExecutor[F[_]: Sync, Res: GraalVMValuable](
    language: String,
    script:   String
  ): F[Seq[Value] => F[Res]] =
    Sync[F]
      .blocking(ctx.eval(language, script))
      .map { f => args =>
        Sync[F].blocking(f.execute(args: _*)).map(GraalVMValuable[Res].fromGraalValue)
      }

  @simulacrum.typeclass
  sealed trait GraalVMValuable[T] {

    @simulacrum.op("graalValue")
    def toGraalValue(t: T): Value

    def fromGraalValue(value: Value): T
  }

  trait Instances {

    implicit val booleanGraalValuable: GraalVMValuable[Boolean] =
      new GraalVMValuable[Boolean] {
        def toGraalValue(t: Boolean): Value = Value.asValue(t)

        def fromGraalValue(value: Value): Boolean = value.asBoolean()
      }

    implicit val stringGraalValuable: GraalVMValuable[String] =
      new GraalVMValuable[String] {
        def toGraalValue(t: String): Value = Value.asValue(t)

        def fromGraalValue(value: Value): String = value.asString()
      }

    implicit val intGraalValuable: GraalVMValuable[Int] =
      new GraalVMValuable[Int] {
        def toGraalValue(t: Int): Value = Value.asValue(t)

        def fromGraalValue(value: Value): Int = value.asInt()
      }

    implicit val longGraalValuable: GraalVMValuable[Long] =
      new GraalVMValuable[Long] {
        def toGraalValue(t: Long): Value = Value.asValue(t)

        def fromGraalValue(value: Value): Long = value.asLong()
      }

    implicit val floatGraalValuable: GraalVMValuable[Float] =
      new GraalVMValuable[Float] {
        def toGraalValue(t: Float): Value = Value.asValue(t)

        def fromGraalValue(value: Value): Float = value.asFloat()
      }

    implicit val doubleGraalValuable: GraalVMValuable[Double] =
      new GraalVMValuable[Double] {
        def toGraalValue(t: Double): Value = Value.asValue(t)

        def fromGraalValue(value: Value): Double = value.asDouble()
      }

    implicit def mapGraalValuable[K: GraalVMValuable, V: GraalVMValuable]: GraalVMValuable[Map[K, V]] =
      new GraalVMValuable[Map[K, V]] {

        def toGraalValue(t: Map[K, V]): Value = {
          val jMap = new util.HashMap[AnyRef, AnyRef](t.size)
          t.foreach { case (k, v) => jMap.put(GraalVMValuable[K].toGraalValue(k), GraalVMValuable[V].toGraalValue(v)) }
          Value.asValue(jMap)
        }

        def fromGraalValue(value: Value): Map[K, V] =
          asScalaMapIterator(value).map { case (k, v) =>
            GraalVMValuable[K].fromGraalValue(k) -> GraalVMValuable[V].fromGraalValue(v)
          }.toMap
      }

    def asScalaIterator(v: Value): Iterator[Value] =
      if (v.isIterator) {
        Iterator.unfold(v)(i => Option.when(i.hasIteratorNextElement)(i.getIteratorNextElement -> i))
      } else asScalaIterator(v.getIterator)

    def asScalaMapIterator(v: Value): Iterator[(Value, Value)] =
      if (v.isIterator) {
        Iterator.unfold(v)(i =>
          Option.when(i.hasIteratorNextElement) {
            val arr = i.getIteratorNextElement

            (arr.getArrayElement(0) -> arr.getArrayElement(1)) -> i
          }
        )
      } else asScalaMapIterator(v.getHashEntriesIterator)

    implicit def seqGraalValuable[V: GraalVMValuable]: GraalVMValuable[Seq[V]] =
      new GraalVMValuable[Seq[V]] {

        def toGraalValue(t: Seq[V]): Value =
          Value.asValue(t.map(GraalVMValuable[V].toGraalValue).toArray)

        def fromGraalValue(value: Value): Seq[V] =
          asScalaIterator(value).map(GraalVMValuable[V].fromGraalValue).toSeq
      }

    implicit val circeJsonGraalValuable: GraalVMValuable[Json] =
      new GraalVMValuable[Json] {

        private val folder =
          new Json.Folder[Value] {
            def onNull: Value = Value.asValue(null)

            def onBoolean(value: Boolean): Value = Value.asValue(value)

            def onNumber(value: JsonNumber): Value = Value.asValue(value.toDouble)

            def onString(value: String): Value = Value.asValue(value)

            import scala.jdk.CollectionConverters.SeqHasAsJava

            def onArray(value: Vector[Json]): Value = {
              val arr: Array[AnyRef] = new Array(value.size)
              value.zipWithIndex.foreach { case (j, idx) =>
                arr.update(idx, toGraalValue(j))
              }
              val x = Value
                .asValue(ProxyArray.fromArray(arr: _*))
              x
            }

            def onObject(value: JsonObject): Value = {
              val map = new java.util.HashMap[String, AnyRef](value.size)
              value.toMap.foreach { case (key, value) =>
                map.put(key, toGraalValue(value))
              }
              Value.asValue(ProxyObject.fromMap(map))
            }
          }

        def toGraalValue(t: Json): Value =
          t.foldWith(folder)

        def fromGraalValue(value: Value): Json =
          if (value.isNull) Json.Null
          else if (value.isBoolean) value.asBoolean().asJson
          else if (value.isNumber) {
            if (value.fitsInInt()) value.asInt().asJson
            else if (value.fitsInLong()) value.asLong().asJson
            else if (value.fitsInFloat()) value.asFloat().asJson
            else value.asDouble().asJson
          } else if (value.isString) value.asString().asJson
          else if (value.hasArrayElements)
            Json.arr(asScalaIterator(value).map(fromGraalValue).toSeq: _*)
          else if (value.hasHashEntries) {
            Json.obj(
              asScalaMapIterator(value).map { case (k, v) =>
                k.asString() -> fromGraalValue(v)
              }.toSeq: _*
            )
          } else throw new MatchError(value)
      }

    implicit def asGraalValue[T: GraalVMValuable](t: T): Value = GraalVMValuable[T].toGraalValue(t)

//    implicit class StringOps(string: String) {
//
//      def jsFunction[F[_]: Sync, Res: GraalVMValuable]: F[ScriptFunction[F, Res]] =
//        jsExecutor[F, Res](string).map(new ScriptFunction(_))
//    }
  }

  object instances extends Instances

  class ScriptFunction[F[_]: Sync, Res: GraalVMValuable](executor: Seq[Value] => F[Res]) {
    import instances._
    def apply(): F[Res] = executor(Nil)
    def apply[Arg0: GraalVMValuable](arg0: Arg0): F[Res] = executor(List(arg0))

    def apply[Arg0: GraalVMValuable, Arg1: GraalVMValuable](arg0: Arg0, arg1: Arg1): F[Res] = executor(
      List(arg0, arg1)
    )

    def apply[Arg0: GraalVMValuable, Arg1: GraalVMValuable, Arg2: GraalVMValuable](
      arg0: Arg0,
      arg1: Arg1,
      arg2: Arg2
    ): F[Res] = executor(List(arg0, arg1, arg2))
  }

}
