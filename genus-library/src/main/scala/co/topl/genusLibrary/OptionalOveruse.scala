package co.topl.genusLibrary

object OptionalOveruse {

  object BasicBadPractice {

    // what is the problem with this code?
    // => id is not optional, it's missing on creation but always present otherwise
    case class User(id: Option[String], name: String, email: String)

  }

  object RealBadPractice {

    case class User(
      id:      Option[String],
      name:    String,
      email:   Option[String] = None,
      friends: Seq[String] = Seq(),
      admin:   Boolean = false
    )

  }

  object SuggestedPractice {

    // create dedicated classes for the use context

    case class UserCreation(name: String, email: String)

    case class User(id: String, name: String, email: String)

    case class UserFull(id: String, name: String, email: String, friends: Seq[String], admin: Boolean)

  }

}
