package co.topl.networking

import cats.Order
import co.topl.crypto.hash.Blake2b256
import co.topl.models.Bytes

import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets

package object p2p {

  // TODO: Compare on full address
  implicit val orderInetSocketAddress: Order[InetSocketAddress] =
    Order.from((a, b) =>
      BigInt(
        new Blake2b256()
          .hash(
            Bytes(a.getPort.toString.getBytes(StandardCharsets.UTF_8)),
            Bytes(b.getPort.toString.getBytes(StandardCharsets.UTF_8))
          )
          .data
          .toArray
      ).compare(
        BigInt(
          new Blake2b256()
            .hash(
              Bytes(b.getPort.toString.getBytes(StandardCharsets.UTF_8)),
              Bytes(a.getPort.toString.getBytes(StandardCharsets.UTF_8))
            )
            .data
            .toArray
        )
      )
    )
}
