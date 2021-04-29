package co.topl.utils

import java.io.File
import java.nio.file.Files.createTempDirectory
import java.nio.file.{Files, Path}

import scala.util.Random

trait FileUtils {

  private val basePath: Path = Files.createTempDirectory(s"bifrost-${System.nanoTime()}")
  private val prefixLength = 10

  sys.addShutdownHook {
    removeDir(basePath)
  }

  def createTempFile: File = {
    val dir = createTempDir
    val prefix = Random.alphanumeric.take(prefixLength).mkString
    val suffix = Random.alphanumeric.take(prefixLength).mkString
    val file = Files.createTempFile(dir.toPath, prefix, suffix).toFile
    file.deleteOnExit()
    file
  }

  def createTempDir: File = {
    val prefix = Random.alphanumeric.take(prefixLength).mkString
    val file = createTempDirectory(basePath, prefix).toFile
    file.deleteOnExit()
    file
  }

  def removeDir(path: Path): Unit = {
    def deleteRecursive(dir: File): Unit =
      for (file <- dir.listFiles) {
        if (file.isDirectory) {
          deleteRecursive(file)
        }
        file.delete()
      }
    deleteRecursive(path.toFile)
  }
}
