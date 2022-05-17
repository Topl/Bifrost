package co.topl.genus

package object ops {

  trait Implicits
      extends SourceOps.ToSourceOps
      with FunctionKOps.ToFunctionKOps
      with protobufops.Implicits
      with QueryServiceOps.ToOps
      with SourceCompanionOps.ToOps

  object implicits extends Implicits
}
