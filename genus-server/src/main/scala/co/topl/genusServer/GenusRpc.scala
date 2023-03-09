package co.topl.genusServer

trait GenusRpc[F[_]]{

  def helloWorld(): F[String]

}
