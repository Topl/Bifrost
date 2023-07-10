package co.topl.node.cli

import cats._
import cats.implicits._

import scala.annotation.tailrec

/**
 * To support interactivity in the CLI, we need to capture the computation for a stage/phase and determine what to do next.
 * For example, a user may provide input that requires a follow-up question.  Or, a user may provide an input that requires
 * the program to quit immediately.  Or perhaps the processing is complete for the current command and we need to return
 * the user back to the main menu.
 * @tparam A the result type of the current stage
 */
sealed abstract class StageResult[+A]

object StageResult {

  /**
   * Indicates the program should exit upon completion of the current stage.
   */
  case object Exit extends StageResult[Nothing]

  /**
   * Indicates the program should return to main menu upon completion of the current stage.
   */
  case object Menu extends StageResult[Nothing]

  /**
   * Indicates the program has more processing to do using the result value of the current stage.
   */
  case class Success[+A](a: A) extends StageResult[A]

  implicit val catsStdInstancesForStageResult: Traverse[StageResult] with MonadError[StageResult, Unit] =
    new Traverse[StageResult] with MonadError[StageResult, Unit] {

      def flatMap[A, B](fa: StageResult[A])(f: A => StageResult[B]): StageResult[B] =
        fa match {
          case StageResult.Success(a) => f(a)
          case StageResult.Exit       => StageResult.Exit
          case StageResult.Menu       => StageResult.Menu
        }

      def pure[A](x: A): StageResult[A] = StageResult.Success(x)

      @tailrec
      def tailRecM[A, B](a: A)(f: A => StageResult[Either[A, B]]): StageResult[B] =
        f(a) match {
          case StageResult.Success(Left(a))  => tailRecM(a)(f)
          case StageResult.Success(Right(b)) => StageResult.Success(b)
          case StageResult.Exit              => StageResult.Exit
          case StageResult.Menu              => StageResult.Menu
        }

      def raiseError[A](e: Unit): StageResult[A] = StageResult.Exit

      def handleErrorWith[A](fa: StageResult[A])(f: Unit => StageResult[A]): StageResult[A] =
        fa match {
          case StageResult.Success(a) => StageResult.Success(a)
          case _                      => f(())
        }

      def traverse[G[_]: Applicative, A, B](fa: StageResult[A])(f: A => G[B]): G[StageResult[B]] =
        fa match {
          case StageResult.Success(a) => f(a).map(StageResult.Success(_))
          case StageResult.Exit       => Applicative[G].pure(StageResult.Exit)
          case StageResult.Menu       => Applicative[G].pure(StageResult.Menu)
        }

      def foldLeft[A, B](fa: StageResult[A], b: B)(f: (B, A) => B): B =
        fa match {
          case StageResult.Success(a) => f(b, a)
          case _                      => b
        }

      def foldRight[A, B](fa: StageResult[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa match {
          case StageResult.Success(a) => f(a, lb)
          case _                      => lb
        }
    }
}

/**
 * Assists with using a StageResult in an F[_] context.  Similar to EitherT or OptionT.
 */
final case class StageResultT[F[_], A](value: F[StageResult[A]]) {

  def map[B](f: A => B)(implicit F: Functor[F]): StageResultT[F, B] =
    StageResultT(F.map(value)(_.map(f)))

  def flatMap[B](f: A => StageResultT[F, B])(implicit F: Monad[F]): StageResultT[F, B] =
    flatMapF(a => f(a).value)

  def flatMapF[B](f: A => F[StageResult[B]])(implicit F: Monad[F]): StageResultT[F, B] =
    StageResultT(
      F.flatMap(value) {
        case StageResult.Success(a) => f(a)
        case StageResult.Exit       => F.pure(StageResult.Exit)
        case StageResult.Menu       => F.pure(StageResult.Menu)
      }
    )

  def flatTap[B](f: A => StageResultT[F, B])(implicit F: Monad[F]): StageResultT[F, A] =
    flatMap(a => f(a).map(_ => a))

  def >>[B](fb: => StageResultT[F, B])(implicit F: Monad[F]): StageResultT[F, B] =
    flatMap(_ => fb)

  def >>=[B](f: A => StageResultT[F, B])(implicit F: Monad[F]): StageResultT[F, B] =
    flatMap(f)
}
