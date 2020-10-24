package utils

import java.io.File
import java.nio.file.Files.createTempDirectory
import java.nio.file.{ Files, Path }

import scala.util.Random

trait FileUtils {

  private val basePath: Path = java.nio.file.Files.createTempDirectory(s"bifrost-${System.nanoTime()}")
  private val prefixLength = 10

  def createTempFile: File = {
    val dir = createTempDir
    val prefix = Random.alphanumeric.take(prefixLength).mkString
    val suffix = Random.alphanumeric.take(prefixLength).mkString
    val file = Files.createTempFile(dir.toPath, prefix, suffix).toFile
    file.deleteOnExit()
    file
  }

  private def createTempDir: File = {
    val prefix = Random.alphanumeric.take(prefixLength).mkString
    val file = createTempDirectory(basePath, prefix).toFile
    file.deleteOnExit()
    file
  }
}
