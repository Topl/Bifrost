package co.topl.api.transaction

import akka.util.ByteString
import co.topl.api.RPCMockState
import co.topl.attestation.Address
import org.scalatest.matchers.should.Matchers

class UnprovenArbitTransferRPCHandlerSpec extends RPCMockState with Matchers {}

object UnprovenArbitTransferRPCHandlerSpec {

  /**
   * Creates an Unproven Arbit Transfer request HTTP body.
   * @param propositionType the type of proposition used for signing the transfer
   * @param sender the address that polys should be sent from
   * @param recipient the recipient of the polys
   * @param amount the amount of polys to send
   * @param fee the fee provided for the transaction
   * @return a [[ByteString]] representing the HTTP body
   */
  def createRequestBody(
    propositionType: String,
    sender:          Address,
    recipient:       Address,
    amount:          Int,
    fee:             Int
  ): ByteString =
    ByteString(s"""
                  |{
                  | "jsonrpc": "2.0",
                  | "id": "2",
                  | "method": "topl_unprovenArbitTransfer",
                  | "params": [{
                  |   "propositionType": "$propositionType",
                  |   "recipients": [["$recipient", "$amount"]],
                  |   "sender": ["$sender"],
                  |   "changeAddress": "$sender",
                  |   "consolidationAddress": "$sender",
                  |   "minting": "false",
                  |   "fee": "$fee",
                  |   "data": "",
                  |   "boxSelectionAlgorithm": "All"
                  | }]
                  |}
    """.stripMargin)
}
