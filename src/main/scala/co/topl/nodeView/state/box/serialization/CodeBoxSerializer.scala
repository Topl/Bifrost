package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box.CodeBox
import co.topl.utils.Extensions._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object CodeBoxSerializer extends BifrostSerializer[CodeBox] {

  override def serialize(obj: CodeBox, w: Writer): Unit = {
    ProgramBoxSerializer.serialize(obj, w)

    /* code: Seq[String], List of strings of JS functions */
    w.putUInt(obj.code.length)
    obj.code.foreach(js => w.putIntString(js))

    /* interface: Map[String, Seq[String]] */
    w.putUInt(obj.interface.size)
    obj.interface.foreach { case (methodName, params) =>
      w.putIntString(methodName)
      w.putUInt(params.length)
      params.foreach(p => w.putIntString(p))
    }
  }

  override def parse(r: Reader): CodeBox = {
    val (proposition, nonce, programId) = ProgramBoxSerializer.parse(r)

    /* code: Seq[String], List of strings of JS functions */
    val codeLength: Int = r.getUInt().toIntExact
    val code: Seq[String] = (0 until codeLength).map(_ => r.getIntString())

    /* interface: Map[String, Seq[String]] */
    val interfaceSize: Int = r.getUInt().toIntExact

    val interface: Map[String, Seq[String]] = (0 until interfaceSize).map { _ =>
      val methodName: String = r.getIntString()
      val paramsLength: Int = r.getUInt().toIntExact
      val params: Seq[String] = (0 until paramsLength).map(_ => r.getIntString())
      methodName -> params
    }.toMap

    CodeBox(proposition, nonce, programId, code, interface)
  }
}
