package co.topl.genusLibrary.orientDb

import cats.effect.{IO, Resource, Sync, SyncIO}
import com.orientechnologies.orient.core.db.{ODatabaseType, OrientDB, OrientDBConfigBuilder}
import munit.{CatsEffectSuite, FunSuite, TestOptions}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait DbFixtureUtil { self: FunSuite with CatsEffectSuite =>

  type F[A] = IO[A]
  implicit val logger: Logger[F] = Slf4jLogger.getLoggerFromClass[F](this.getClass)

  val orientDbFixture: SyncIO[FunFixture[OrientDB]] = {
    val dbName = "testDb"
    val dbConfig = new OrientDBConfigBuilder().addGlobalUser("testUser", "testPass", "*").build()

    val factoryR =
      Resource.make[F, OrientDB](Sync[F].blocking(new OrientDB("memory", "root", "root", dbConfig)))(oDb =>
        Sync[F].delay(oDb.close())
      )

    def setup(t: TestOptions, odb: OrientDB): IO[Unit] =
      Sync[F]
        .blocking(odb.createIfNotExists(dbName, ODatabaseType.MEMORY))
        .flatMap(res => logger.info(s"Db Created $res"))

    def teardown(odb: OrientDB): IO[Unit] =
      Sync[F]
        .blocking(odb.drop(dbName))
        .flatTap(_ => logger.info("Teardown, db dropped"))

    ResourceFixture[OrientDB](factoryR, setup _, teardown _)
  }

}
