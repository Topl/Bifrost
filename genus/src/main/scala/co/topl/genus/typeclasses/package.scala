package co.topl.genus

package object typeclasses {

  trait Implicits
      extends TransformInstances
      with Transform.ToTransformOps
      with MongoFilterInstances
      with MongoFilter.ToMongoFilterOps

  object implicits extends Implicits
}
