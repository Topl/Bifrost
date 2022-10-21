package co.topl.genusLibrary

import co.topl.genusLibrary.orientDb.OrientDBFacade

import java.io.{File, FileInputStream, FileOutputStream}
import java.nio.charset.Charset
import java.nio.file.Files

class OrientDBFacadeTest extends munit.FunSuite {
  val testDir: File = Files.createTempDirectory("ODbF").toFile

  def randomFile(): File =
    new File(testDir, "TF" + Math.random().toString)

  test("ensureDirectoryExists - It does") {
    val dir = randomFile()
    assert(dir.mkdir(), s"Failed to create test directory $dir")
    OrientDBFacade.ensureDirectoryExists(dir)
    assert(dir.delete(), s"Failed to delete test directory $dir")
  }

  test("ensureDirectoryExists - It doesn't") {
    val dir = randomFile()
    OrientDBFacade.ensureDirectoryExists(dir)
    assert(dir.isDirectory, s"ensureDirectoryExists did not create $dir")
    assert(dir.delete(), s"Failed to delete test directory $dir")
  }

  test("ensureDirectoryExists - not a directory") {
    val dir = randomFile()
    new FileOutputStream(dir).close()
    assert(dir.isFile, s"Attempt to write file named $dir did not work")
    intercept[GenusException] {
      OrientDBFacade.ensureDirectoryExists(dir)
    }
    assert(dir.delete(), s"Failed to delete test file $dir")
  }

  test("setupOrientDBEnvironment") {
    val dir = randomFile()
    val pwdFilePath = OrientDBFacade.passwordFile(dir)
    assert(!pwdFilePath.exists(), "Password file should not exist yet")
    val facade = OrientDBFacade.setupOrientDBEnvironment(dir)
    assert(facade.isSuccess, "setupOrientDBEnvironment should have succeeded")
    assert(pwdFilePath.exists(), s"There must be a file named ${pwdFilePath.getAbsolutePath}")
    val buffer: Array[Byte] = Array.ofDim(pwdFilePath.length().toInt)
    val inputStream = new FileInputStream(pwdFilePath)
    val byteCount = inputStream.read(buffer)
    inputStream.close()
    val expectedPassword = new String(buffer, 0, byteCount, Charset.forName("UTF-8"))
    assertEquals(
      OrientDBFacade.rootPassword(pwdFilePath).get,
      expectedPassword,
      "Password read by test is different than the one returned by rootPassword"
    )
    val dbServerConfigFile = new File(new File(dir, "config"), "orientdb-server-config.xml")
    assert(dbServerConfigFile.isFile, "The DB server config file should have been created.")
  }
}
