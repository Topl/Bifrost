package co.topl.models

case class TaktikosAddress(
  paymentVerificationKeyHash: Sized.Strict[Bytes, Lengths.`32`.type],
  stakingVerificationKey:     Sized.Strict[Bytes, Lengths.`32`.type],
  signature:                  Sized.Strict[Bytes, Lengths.`64`.type]
)
