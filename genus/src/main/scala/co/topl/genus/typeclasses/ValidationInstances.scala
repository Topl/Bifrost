package co.topl.genus.typeclasses

import cats.implicits._
import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import co.topl.genus.types.BlockHeight
import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.algebras.SubscriptionServiceAlg.CreateRequest
import co.topl.genus.services.services_types.Paging

trait ValidationInstances {

  implicit val pagingValidation: Validation[Paging] =
    paging =>
      Validated
        .condNec(paging.pageSize >= 0, paging, "page size can not be negative")
        .andThen(_ => Validated.condNec(paging.pageNumber >= 0, paging, "page number can not be negative"))

  implicit def queryRequestValidation[Filter, Sort]: Validation[QueryRequest[Filter, Sort]] =
    request =>
      combineValidation(
        validatePaging(request.paging),
        validateConfirmationDepth(request.confirmationDepth)
      )(request)

  implicit def createRequestValidation[Filter]: Validation[CreateRequest[Filter]] =
    request =>
      combineValidation(
        validateConfirmationDepth(request.confirmationDepth),
        request.startFromHeight
          .map(validateHeight.validate)
          .map(_.map(_.some))
          .getOrElse(Validated.validNec(None))
      )(request)

  implicit def validateHeight: Validation[BlockHeight] =
    height => Validated.condNec(height.value >= 1, height, "height must be at least 1")

  private def validatePaging(pagingOpt: Option[Paging]): ValidatedNec[String, Option[Paging]] =
    pagingOpt
      .map(paging => pagingValidation.validate(paging).map(_.some))
      .getOrElse(Validated.validNec[String, Option[Paging]](None))

  private def validateConfirmationDepth(depth: Int): ValidatedNec[String, Int] =
    Validated.condNec(depth >= 0, depth, "confirmation depth can not be negative")

  /**
   * Combines the validation of multiple peices of data. Returns a specified value when valid.
   *
   * This function operates like the cats `mapN` function, but is compatible with both Scala 2.12 and 2.13.
   *
   * @param validated a set of validated values
   * @param ifValid a valid value to return if all validation passes
   * @tparam E the validation error type
   * @tparam A the validation success type
   * @return a [[ValidatedNec]] containing either a list of invalid reasons or a valid result
   */
  private def combineValidation[E, A](validated: ValidatedNec[E, Any]*)(ifValid: => A): ValidatedNec[E, A] =
    validated.foldLeft(Validated.validNec[E, A](ifValid)) {
      case (Validated.Invalid(reasons), Validated.Invalid(nextReasons)) =>
        Validated.invalid[NonEmptyChain[E], A](reasons ++ nextReasons)
      case (_, Validated.Invalid(reasons)) =>
        Validated.invalid[NonEmptyChain[E], A](reasons)
      case (valid, _) => valid
    }
}
