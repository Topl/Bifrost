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

  def exit[A]: StageResult[A] = Exit

  /**
   * Indicates the program should return to main menu upon completion of the current stage.
   */
  case object Menu extends StageResult[Nothing]
  def menu[A]: StageResult[A] = Menu

  /**
   * Indicates the program has more processing to do using the result value of the current stage.
   */
  case class Success[+A](a: A) extends StageResult[A]
  def success[A](a: A): StageResult[A] = Success(a)

  implicit val catsStdInstancesForStageResult: Monad[StageResult] =
    new Monad[StageResult] {

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

  def semiflatMap[B](f: A => F[B])(implicit F: Monad[F]): StageResultT[F, B] =
    StageResultT(
      F.flatMap(value) {
        case StageResult.Success(a) => f(a).map(StageResult.success)
        case StageResult.Exit       => F.pure(StageResult.Exit)
        case StageResult.Menu       => F.pure(StageResult.Menu)
      }
    )

  def subflatMap[B](f: A => StageResult[B])(implicit F: Monad[F]): StageResultT[F, B] =
    StageResultT(
      F.flatMap(value) {
        case StageResult.Success(a) => f(a).pure[F]
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

object StageResultT {

  def liftF[F[_], A](fa: F[A])(implicit F: Functor[F]): StageResultT[F, A] =
    StageResultT(fa.map(StageResult.Success(_)))

  implicit def stageResultTMonadInstance[F[_]: Monad]: Monad[StageResultT[F, *]] =
    new Monad[StageResultT[F, *]] {
      def pure[A](x: A): StageResultT[F, A] = StageResultT.liftF(x.pure[F])

      def flatMap[A, B](fa: StageResultT[F, A])(f: A => StageResultT[F, B]): StageResultT[F, B] =
        StageResultT(
          Monad[F].flatMap(fa.value) {
            case StageResult.Success(a) => f(a).value
            case StageResult.Exit       => Monad[F].pure(StageResult.Exit)
            case StageResult.Menu       => Monad[F].pure(StageResult.Menu)
          }
        )

      def tailRecM[A, B](a: A)(f: A => StageResultT[F, Either[A, B]]): StageResultT[F, B] =
        StageResultT(
          f(a).value.flatMap {
            case StageResult.Success(Left(a))  => tailRecM(a)(f).value
            case StageResult.Success(Right(b)) => StageResult.success(b).pure[F]
            case StageResult.Menu              => StageResult.menu[B].pure[F]
            case StageResult.Exit              => StageResult.exit[B].pure[F]
          }
        )
    }
}
