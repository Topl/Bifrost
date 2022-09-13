package co.topl.node

import com.typesafe.config.{Config, ConfigFactory}

import java.nio.file.{Files, Path}

object YamlConfig {

  def parse(data: String): Config = {
    val json = io.circe.yaml.parser.parse(data) match {
      case Left(e)      => throw e
      case Right(value) => value
    }
    ConfigFactory.parseString(json.toString)
  }

  def load(path: Path): Config = {
    val data = Files.readString(path)
    parse(data)
  }

  def loadResource(name: String): Config = {
    val source = scala.io.Source.fromResource(name)
    val data =
      try source.mkString
      finally source.close()
    parse(data)
  }
}
