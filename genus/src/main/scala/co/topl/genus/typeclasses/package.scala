package co.topl.genus

package object typeclasses {

  trait Implicits
      extends TransformInstances
      with Transform.ToTransformOps
      with MongoFilterInstances
      with MongoFilter.ToMongoFilterOps
      with FunctionKInstances

  object implicits extends Implicits
}
