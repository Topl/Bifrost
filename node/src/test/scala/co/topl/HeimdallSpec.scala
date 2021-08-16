package co.topl

import akka.actor.testkit.typed.scaladsl.{ManualTime, ScalaTestWithActorTestKit}
import co.topl.nodeView.NodeViewTestHelpers
import co.topl.utils.{CommonGenerators, InMemoryKeyFileTestHelper, TestSettings}
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpecLike
import org.scalatest.{Inspectors, OptionValues}

import java.nio.file.Files

class HeimdallSpec
    extends ScalaTestWithActorTestKit(ManualTime.config.withFallback(TestSettings.defaultConfig))
    with AnyFlatSpecLike
    with TestSettings
    with InMemoryKeyFileTestHelper
    with NodeViewTestHelpers
    with MockFactory
    with OptionValues
    with Inspectors
    with CommonGenerators {

  behavior of "Heimdall"

  it should "crash if a child actor crashes" in {
    val ref = spawn(
      Heimdall.apply(
        settings.copy(application =
          // Setting `keyFileDir` to None results in a KeyManager exception
          settings.application.copy(keyFileDir = None)
        ),
        appContext
      )
    )

    createTestProbe().expectTerminated(ref)
  }

  it should "crash if NodeViewHolder fails initialization" in {
    val ref = spawn(
      Heimdall.apply(
        settings.copy(application =
          // Setting `dataDir` to None results in a NodeViewHolder exception
          settings.application.copy(
            dataDir = Some(Files.createTempFile("HeimdallSpecDataDirAsFile", "").toString)
          )
        ),
        appContext
      )
    )

    createTestProbe().expectTerminated(ref)
  }

}
