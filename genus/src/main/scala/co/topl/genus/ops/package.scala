package co.topl.genus

package object ops {

  trait Implicits extends SourceOps.ToSourceOps with FunctionKOps.ToFunctionKOps with protobufops.Implicits

  object implicits extends Implicits
}
