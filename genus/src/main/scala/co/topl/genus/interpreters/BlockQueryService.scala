package co.topl.genus.interpreters

import co.topl.genus.services.block_query.{BlockQuery, QueryBlockReq, QueryBlockRes}
import co.topl.genus.types.Block

import scala.concurrent.Future

object BlockQueryService {

  object Mock {

    def make: BlockQuery =
      (_: QueryBlockReq) =>
        Future.successful(
          QueryBlockRes(
            List(
              Block(
                id = "test-block-id-1",
                height = 44
              ),
              Block(
                id = "test-block-id-2",
                height = 99
              )
            )
          )
        )
  }
}
