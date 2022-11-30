# Local Testnet Simulator Guide
Instructions for running a testnet simulation in a local environment.

## Installation
### Docker, Java, and SBT
1. Install [Docker](https://docs.docker.com/get-docker/).
1. Install [Java](https://adoptium.net/installation/).
1. Install [SBT](https://www.scala-sbt.org/download.html).

NOTE: Java and SBT can be installed using [SDKMAN](https://sdkman.io/install)
1. Run `curl -s "https://get.sdkman.io" | bash`
1. Run `source "$HOME/.sdkman/bin/sdkman-init.sh"`
1. Run `sdk install java 11.0.17-tem`
1. Run `sdk install sbt 1.7.3`

### Kubernetes
1. Install [Microk8s](https://microk8s.io/docs/install-alternatives).
1. Enable addons with command `microk8s enable dns helm helm3 hostpath-storage registry storage`

### Bifrost
1. Clone https://github.com/Topl/Bifrost and checkout branch `tetra`.
1. Run `sbt "Docker / publishLocal"` from the command line.
1. Run `docker tag toplprotocol/network-delayer localhost:32000/topl/network-delayer && docker push localhost:32000/topl/network-delayer`
1. Run `docker tag toplprotocol/bifrost-node-tetra localhost:32000/topl/bifrost-node-tetra && docker push localhost:32000/topl/bifrost-node-tetra`
1. Run `docker tag toplprotocol/testnet-simulation-orchestrator localhost:32000/topl/testnet-simulation-orchestrator && docker push localhost:32000/topl/testnet-simulation-orchestrator`.

## Run Simulation
1. Open terminal.  `cd {bifrost repository}/helm`
1. Copy `bifrost-consensus-testnet/override-example.yaml` to a path on your machine.  Update values accordingly.
1. From terminal, run `microk8s helm install -f {path to override.yaml} --create-namespace --namespace {scenario name} {scenario name} ./bifrost-consensus-testnet`
1. Wait for the test run to complete.  You can observe its progress by accessing the logs of the running pods.  [k9s](https://k9scli.io/) is a helpful tool but requires [kubeconfig setup](https://microk8s.io/docs/working-with-kubectl).
1. View the [results](https://console.cloud.google.com/storage/browser/bifrost-topl-labs-testnet-scenario-results/%2Fsimulation/results)
