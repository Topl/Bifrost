# load-testing

Load testing application for a Bifrost node.

Generates users with an address that trade Polys and Assets among themselves.

## Usage

```
sbt run co.topl.loadtesting.LoadTestingApp
  -n --network <network>        // the Bifrost network ("private" or "valhalla")
  -c --userCount <int>          // the number of users to generate
  -u --uri <str>                // the URI of the Bifrost node
  -a --apiKey <str>             // the API key of the Bifrost node
  -s --seed <str>               // the seed used to generate user keys/addresses -- default = "test"
  -o --statisticsPath <str>     // the output path for statistics -- default = ./stats.csv
  -t --timeoutSeconds <int>     // the time to wait for akka actor responses -- default = 30
```

