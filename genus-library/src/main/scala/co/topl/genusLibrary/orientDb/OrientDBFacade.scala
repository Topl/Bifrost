package co.topl.genusLibrary.orientDb

import cats.data.EitherT
import cats.effect.implicits.effectResourceOps
import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.genusLibrary.orientDb.wrapper.{GraphTxWrapper, WrappedVertex}
import co.topl.genusLibrary.GenusException
import co.topl.typeclasses.Log._
import com.tinkerpop.blueprints.impls.orient.{OrientGraph, OrientGraphFactory, OrientGraphNoTx}
import org.typelevel.log4cats.Logger
//import com.typesafe.scalalogging.Logger
import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.charset.Charset
import scala.annotation.unused
import scala.collection.mutable
import scala.jdk.javaapi.CollectionConverters.asScala
import scala.util.{Random, Success, Try}

/**
 * This is a class to hide the details of interacting with OrientDB.
 */
//private[genusLibrary] class OrientDBFacade(dir: File, password: String) extends StoreFacade {
private[genusLibrary] class OrientDBFacade(dir: File, password: String) {
  import OrientDBFacade._
//  logger.info("Starting OrientDB with DB server")
//  private val dbUserName = "admin"

//  logger.debug("creating graph factory")
//  private val factory = new OrientGraphFactory("plocal:" + dir.getAbsolutePath, dbUserName, password)

//  logger.debug("Embed the Server")

//  @unused
//  private val genusGraphMetadata = initializeDatabase(factory, password)

  // TODO remove this
//   def getGraph[F[_]: Async: org.typelevel.log4cats.Logger]: GraphTxDAO[F] = ???
//    new GraphTxDAO[F](
//      new GraphTxWrapper(factory.getTx)
//    )

//  override def getGraphNoWrapper: OrientGraph = factory.getTx

//  override def getGraphNoTx: OrientGraphNoTx = factory.getNoTx

//  override def getVertexByField[F[_]: Async](
//    vertexTypeName: VertexTypeName,
//    filterKey:      PropertyKey,
//    filterValue:    AnyRef
//  ): F[Option[WrappedVertex]] =
//    getVertexByFields(vertexTypeName, Set((filterKey, filterValue)))
//
//  override def getVertexByFields[F[_]: Async](
//    vertexTypeName:   VertexTypeName,
//    propertiesFilter: Set[PropertyQuery]
//  ): F[Option[WrappedVertex]] =
//    getVerticesByFields(vertexTypeName, propertiesFilter)
//      .map(_.headOption)
//
//  override def getVerticesByField[F[_]: Async](
//    vertexTypeName: VertexTypeName,
//    filterKey:      PropertyKey,
//    filterValue:    AnyRef
//  ): F[Iterable[WrappedVertex]] =
//    getVerticesByFields(vertexTypeName, Set((filterKey, filterValue)))
//
//  override def getVerticesByFields[F[_]: Async](
//    vertexTypeName:   VertexTypeName,
//    propertiesFilter: Set[PropertyQuery]
//  ): F[Iterable[WrappedVertex]] = Async[F].delay {
//    val (keys, values) = propertiesFilter.foldLeft((List.empty[PropertyKey], List.empty[Object])) {
//      case ((keys, values), (currentKey, currentValue)) => (currentKey :: keys, currentValue :: values)
//    }
//    asScala(
//      genusGraphMetadata.graphNoTx.getVertices(vertexTypeName, keys.toArray, values.toArray)
//    )
//      .map(new WrappedVertex(_))
//  }

  // TODO Fix the password logic, why is used a no transactional db, and then commit is being applied?
  private def initializeDatabase(factory: OrientGraphFactory, password: String) = {
    val session: OrientGraphNoTx = factory.getNoTx
//    try {
//      logger.info("Changing password")
//      session.command(new OCommandSQL(s"UPDATE OUser SET password='$password' WHERE name='$dbUserName'")).execute()
//      logger.info("Configuring Schema")
//    new GenusGraphMetadata(session)
//    } finally
//      session.commit()
//      session.shutdown()
  }

//  def shutdown(): Boolean =
//    Try {
//      factory.close()
//    }.logIfFailure("Error while shutting down database").isSuccess
}

object OrientDBFacade {
//  implicit private val logger: Logger = Logger(classOf[OrientDBFacade])

  private val charsetUtf8: Charset = Charset.forName("UTF-8")
  private val passwdLength = 30

  import cats.effect.Sync

  def make[F[_]: Sync: Logger](directoryPath: String, user: String, password: String): Resource[F, OrientGraphFactory] =
    for {
      directory <- Sync[F].blocking(new File(directoryPath)).toResource
      _ <- Either
        .cond(
          directory.isDirectory || directory.mkdir(),
          Resource.unit[F],
          GenusException(s"${directory.getAbsolutePath} exists but is not a directory, or it can not be created.")
        )
        .leftMap(e => println(e.getMessage)) // TODO, remove println
        .valueOr(_ => Resource.canceled)

      // Start Orient Db embedded server
      orientGraphFactory <- Resource.make[F, OrientGraphFactory](
        Sync[F].delay(
          new OrientGraphFactory(s"plocal:${directory.getAbsolutePath}", user, password)
        )
      )(factory => Sync[F].delay(factory.close()))

      // Metadata describing the schema used for the Genus graph in OrientDB
      _ <- GenusGraphMetadata.make(orientGraphFactory.getNoTx).toResource.void

    } yield orientGraphFactory

  /**
   * Create an instance of OrientDBFacade
   *
   * @return the new instance
   */
//  private def apply(orientDbDirectory: String = "./genus_db"): OrientDBFacade = {
//    val dir = new File(orientDbDirectory)
//    setupOrientDBEnvironment(dir)
//      .flatMap(password => Success(new OrientDBFacade(dir, password)))
//      .recover(excp => throw excp)
//      .get
//  }

//  private[genusLibrary] def ensureDirectoryExists(directory: File): Unit =
//    if (!directory.isDirectory)
//      if (directory.exists)
//        throw GenusException(s"${directory.getAbsolutePath} exists but is not a directory.")
//      else if (!directory.mkdir())
//        throw GenusException(s"Failed to create directory ${directory.getAbsolutePath}")
//      else logger.debug("Using existing directory {}", directory.getAbsolutePath)

//  private[genusLibrary] def setupOrientDBEnvironment(dbDirectory: File): Try[String] = {
//    ensureDirectoryExists(dbDirectory)
//    System.setProperty("ORIENTDB_HOME", dbDirectory.getAbsolutePath)
//    scala.util.Try.apply("admin")
////    rootPassword(passwordFile(dbDirectory))
////      .logIfFailure("Failed to read password")
//  }

//  private[genusLibrary] def passwordFile(dir: File) = new File(dir, "root_pwd")

  /**
   * Read the database root password from a file. If the file does not exist then create the file with a random
   * password.
   *
   * @param pwdFilePath The path of the file that that random password is or will be stored in.
   * @return the database password.
   * @throws java.io.IOException if it needs to write the file but cannot
   */
//  private[genusLibrary] def rootPassword(pwdFilePath: File): Try[String] =
//    ensurePasswordFileExistsAndOpenIt(pwdFilePath)
//      .flatMap { inputStream =>
//        try
//          readPassword(inputStream)
//            .logIfFailure(s"Failed to read password from ${pwdFilePath.getAbsolutePath}")
//        finally
//          inputStream.close()
//      }

//  private def ensurePasswordFileExistsAndOpenIt(pwdFilePath: File): Try[FileInputStream] =
//    Try(new FileInputStream(pwdFilePath))
//      .orElse {
//        writePasswordFile(pwdFilePath)
//        Success(new FileInputStream(pwdFilePath))
//      }
//      .logIfFailure(s"Unable to open ${pwdFilePath.getAbsolutePath}")

//  private def readPassword(inputStream: FileInputStream) = {
//    val buffer: Array[Byte] = Array.ofDim(passwdLength * 2)
//    Try {
//      val bytesRead = inputStream.read(buffer)
//      new String(buffer, 0, bytesRead, charsetUtf8)
//    }
//  }

//  private def writePasswordFile(file: File): Unit = {
//    val outputStream = new FileOutputStream(file)
//    try {
//      val pwdBuilder = new mutable.StringBuilder
//      1 to passwdLength foreach { _ =>
//        pwdBuilder += Random.nextPrintableChar()
//      }
//      val bytes = pwdBuilder.toString().replaceAll("'", "").getBytes(charsetUtf8)
//      logger.debug("writing {} bytes to password file", bytes.length)
//      outputStream.write(bytes)
//    } finally
//      outputStream.close()
//  }
}
