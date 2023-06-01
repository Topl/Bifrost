package co.topl.genusLibrary

import cats.effect.{IO, Resource, SyncIO}
import cats.implicits._
import co.topl.genusLibrary.orientDb.instances.VertexSchemaInstances.instances.{
  blockHeaderSchema,
  ioTransactionSchema,
  txoSchema
}
import co.topl.genusLibrary.orientDb.{OrientDBMetadataFactory, OrientThread}
import com.orientechnologies.orient.core.db.{ODatabaseType, OrientDB, OrientDBConfigBuilder}
import com.tinkerpop.blueprints.impls.orient.OrientGraphFactoryV2
import munit.{CatsEffectSuite, FunSuite, TestOptions}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * @deprecated use DbFixtureUtilV2, and then remove this util
 */
trait DbFixtureUtil { self: FunSuite with CatsEffectSuite =>

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  val orientDbFixture: SyncIO[FunFixture[(OrientDB, OrientThread[F])]] = {
    val dbName = "testDb"
    val dbConfig = new OrientDBConfigBuilder().addGlobalUser("testUser", "testPass", "*").build()

    val factoryR =
      OrientThread
        .create[F]
        .flatMap(orientThread =>
          Resource
            .make[F, OrientDB](orientThread.delay(new OrientDB("memory", "root", "root", dbConfig)))(oDb =>
              orientThread.delay(oDb.close())
            )
            .tupleRight(orientThread)
        )

    def setup(t: TestOptions, odb: (OrientDB, OrientThread[F])): F[Unit] =
      odb._2
        .delay(odb._1.createIfNotExists(dbName, ODatabaseType.MEMORY))
        .flatMap(res => logger.info(s"Db Created $res"))

    def teardown(odb: (OrientDB, OrientThread[F])): IO[Unit] =
      odb._2
        .delay(odb._1.drop(dbName))
        .flatTap(_ => logger.info("Teardown, db dropped"))

    ResourceFixture(factoryR, setup _, teardown _)
  }

}

trait DbFixtureUtilV2 { self: FunSuite with CatsEffectSuite =>

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  val orientDbFixtureV2: SyncIO[FunFixture[(OrientGraphFactoryV2, OrientThread[F])]] = {
    val dbName = "testDb"
    val dbConfig = new OrientDBConfigBuilder()
      .addGlobalUser("testUser", "testPass", "*")
      .build()

    val factoryR =
      OrientThread
        .create[F]
        .flatMap(orientThread =>
          Resource
            .make[F, OrientGraphFactoryV2](
              orientThread
                .delay {
                  val odb = new OrientDB("memory", "root", "root", dbConfig)
                  odb.createIfNotExists(dbName, ODatabaseType.MEMORY)

                  new OrientGraphFactoryV2(odb, dbName, "testUser", "testPass")
                }
            )(oDb => orientThread.delay(oDb.close()))
            .tupleRight(orientThread)
        )

    def setup(t: TestOptions, odb: (OrientGraphFactoryV2, OrientThread[F])): F[Unit] =
      for {
        databaseDocumentTx <- odb._2.delay(odb._1.getNoTx.getRawGraph)
        r <- Seq(
          blockHeaderSchema,
          ioTransactionSchema,
          txoSchema
        )
          .traverse(OrientDBMetadataFactory.createVertex[F](databaseDocumentTx, _))
          .void
      } yield r

    def teardown(odb: (OrientGraphFactoryV2, OrientThread[F])): IO[Unit] =
      odb._2
        .delay(odb._1.drop())
        .flatTap(_ => logger.info("Teardown, db dropped"))

    ResourceFixture(factoryR, setup _, teardown _)
  }

}
