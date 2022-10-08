package co.topl.genusLibrary

import java.io.{File, FileOutputStream}

class OrientDBFacadeTest extends munit.FunSuite {
  def randomFile(): File = {
    new File("TF" + Math.random().toString)
  }

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
}
