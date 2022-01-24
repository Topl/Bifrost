package co.topl.genus.typeclasses

import cats.data.{Validated, ValidatedNec}
import cats.implicits._
import co.topl.genus.algebras.QueryServiceAlg.QueryRequest
import co.topl.genus.services.services_types.Paging

trait ValidationInstances {

  implicit val pagingValidation: Validation[Paging] =
    paging =>
      (
        Validated.condNec(paging.pageSize >= 0, paging, "page size can not be negative"),
        Validated.condNec(paging.pageNumber >= 0, paging, "page number can not be negative")
      ).mapN((_, _) => paging).leftMap(_.map("invalid paging: " + _))

  implicit def queryRequestValidation[Filter, Sort]: Validation[QueryRequest[Filter, Sort]] =
    request =>
      (
        validatePaging(request.paging),
        validateConfirmationDepth(request.confirmationDepth).map(_ => request)
      ).mapN((_, _) => request)

  private def validatePaging(pagingOpt: Option[Paging]): ValidatedNec[String, Option[Paging]] =
    pagingOpt
      .map(paging => pagingValidation.validate(paging).map(_.some))
      .getOrElse(Validated.validNec[String, Option[Paging]](None))

  private def validateConfirmationDepth(depth: Int): ValidatedNec[String, Int] =
    Validated.condNec(depth >= 0, depth, "confirmation depth can not be negative")
}
