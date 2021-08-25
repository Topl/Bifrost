# load-testing

CLI application for load-testing a Bifrost node.

Generates a group of users that trade Polys and Assets among themselves. Transaction results and data
outputs to a CSV file for analysis.

## Usage

```
sbt loadTesting/run
  -n --network <network>        // the Bifrost network ("private" or "valhalla")
  -c --userCount <int>          // the number of users to generate
  -u --uri <str>                // the URI of the Bifrost node
  -a --apiKey <str>             // the API key of the Bifrost node
  -s --seed <str>               // the seed used to generate user keys/addresses -- default = "test"
  -o --statisticsPath <str>     // the output path for statistics -- default = ./stats.csv
  -t --timeoutSeconds <int>     // the time to wait for akka actor responses -- default = 30
```

### Example

To run a load-test with 100 users against a local private network node:

`sbt loadTesting/run -c 100 -n "private" -i "http://localhost:9085" -a "{your api key}"`

## Project Structure

### User Actions

The actions that a user can perform are defined in the `loadtesting.user` package. The current actions are
`SendPolysAction` and `SendAssetsAction`.

A user action is defined as an Akka Streams `Flow`, which takes in the user's current balance as input and returns a
result as output.

A `SendPolysAction` will generate a raw poly transfer, sign it using the actor which manages the key ring,
and send it to the Bifrost node. Once accepted by the node, it will poll on the status of the transaction until
it has been written to a block. Once completed, the action will log information to an output CSV file and the console.

A `SendAssetsAction` is similar to a `SendPolysAction`. It will first generate the raw asset transfer, sign it with the
keys actor, and send it to the Bifrost node. Again, when the node accepts the transaction, the user will poll on the
status until it is completed. Then, the result of the transaction is logged to an output CSV and the console.

Both `SendPolysAction` and `SendAssetsAction` will check that there are enough Polys to create a transaction. In the
case of a `SendAssetsAction`, a new asset will be minted if a user has no assets.

### Guardian Actor

The primary actor in the Akka actor system is the `SimulationActor`. It receives a command to generate a defined
number of users that should run until the actor system is stopped.

Upon initialization, the `SimulationActor` will instantiate a new `KeysActor` which internally instantiates an empty
key ring. The `SimulationActor` will request new addresses from the `KeysActor`, and the `KeysActor` reference is
provided to users for signing capabilities.
