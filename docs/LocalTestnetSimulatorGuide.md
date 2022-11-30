# Local Testnet Simulator Guide
Instructions for running a testnet simulation in a local environment.

## Installation
### Kubernetes
1. Install [Microk8s](https://microk8s.io/docs/install-alternatives).
1. Enable addons with command `microk8s enable dns helm helm3 hostpath-storage registry storage`
1. [Export kubeconfig](https://microk8s.io/docs/working-with-kubectl).
1. Install [Helm](https://helm.sh/docs/intro/install/).

### Bifrost
1. Clone https://github.com/Topl/Bifrost and checkout branch `tetra`.
1. Modify `build.sbt`.  Search for `dockerAliases := dockerAliases.value.flatMap` and add an entry `alias.withRegistryHost(Some("localhost:32000/topl"))` to the list.
1. Run `sbt "Docker / publishLocal"` from the command line.
1. Run `docker push localhost:32000/topl/network-delayer && docker push localhost:32000/topl/bifrost-node-tetra && docker push localhost:32000/topl/testnet-simulation-orchestrator`.

## Run Simulation
1. Open terminal.  `cd {bifrost repository}/helm`
1. Copy `bifrost-consensus-testnet/override-example.yaml` to a path on your machine.  Update values accordingly.
1. From terminal, run `helm install -f {path to override.yaml} --create-namespace --namespace {scenario name} {scenario name} ./bifrost-consensus-testnet`
1. Wait for the test run to complete.  You can observe its progress by accessing the logs of the running pods.  [k9s](https://k9scli.io/) is a helpful tool.
1. View the [results](https://console.cloud.google.com/storage/browser/bifrost-topl-labs-testnet-scenario-results/%2Fsimulation/results)
