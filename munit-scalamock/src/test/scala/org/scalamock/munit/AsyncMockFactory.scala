package org.scalamock.munit

import cats.effect.IO
import org.scalamock.clazz.Mock
import org.scalamock.context.{CallLog, MockContext}
import org.scalamock.function.MockFunctions
import org.scalamock.handlers.{Handlers, OrderedHandlers, UnorderedHandlers}
import org.scalamock.matchers.Matchers

trait AsyncMockFactory extends MockContext with Mock with MockFunctions with Matchers {
  type ExpectationException = AssertionError

  override def newExpectationException(message: String, methodName: Option[Symbol]): AssertionError =
    new AssertionError(message)

  implicit val _factory = this


  def withMock[Res](test: => Res): Res = {
    import cats.effect.unsafe.implicits.global
    withMock(IO.pure(test)).unsafeRunSync()
  }

  def withMock[Res](test: => IO[Res]): IO[Res] = {
    if (expectationContext == null) {
      // we don't reset expectations for the first test case to allow
      // defining expectations in Suite scope and writing tests in OneInstancePerTest/isolated style
      initializeExpectations()
    }

    val testResult = test
      .map { result =>
        verifyExpectations()
        result
      }
      .redeemWith(e => IO(clearExpectations()).flatMap(_ => IO.raiseError[Res](e)), r => IO.pure(r))
    testResult
  }

  protected def inAnyOrder[T](what: => IO[T]): IO[T] =
    inContext(new UnorderedHandlers)(what)

  protected def inSequence[T](what: => IO[T]): IO[T] =
    inContext(new OrderedHandlers)(what)

  def inAnyOrderWithLogging[T](what: => IO[T]): IO[T] =
    inContext(new UnorderedHandlers(logging = true))(what)

  def inSequenceWithLogging[T](what: => IO[T]): IO[T] =
    inContext(new OrderedHandlers(logging = true))(what)

  private def inContext[T](context: Handlers)(what: => IO[T]): IO[T] =
    for {
      previous <- IO.delay {
        currentExpectationContext.add(context)
        val prevContext = currentExpectationContext
        currentExpectationContext = context
        prevContext
      }
      result <- what
      _ <- IO.delay {
        currentExpectationContext = previous
      }
    } yield result

  private def initializeExpectations(): Unit = {
    val initialHandlers = new UnorderedHandlers
    callLog = new CallLog

    expectationContext = initialHandlers
    currentExpectationContext = initialHandlers
  }

  private def clearExpectations(): Unit = {
    // to forbid setting expectations after verification is done
    callLog = null
    expectationContext = null
    currentExpectationContext = null
  }

  private def verifyExpectations(): Unit = {
    callLog.foreach(expectationContext.verify(_))

    val oldCallLog = callLog
    val oldExpectationContext = expectationContext

    clearExpectations()

    if (!oldExpectationContext.isSatisfied)
      reportUnsatisfiedExpectation(oldCallLog, oldExpectationContext)
  }
}
