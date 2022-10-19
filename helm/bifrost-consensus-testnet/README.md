# Usage
1. Copy file `override-example.yaml` into `override.yaml`.  Update values accordingly.
1. Install the helm chart: `helm install -f /path/to/override.yaml --create-namespace --namespace scenario1 scenario1 ./bifrost-consensus-testnet`
1. Observe the logs of each node the Kubernetes cluster in the `scenario1` namespace
1. Terminate the scenario using  `helm delete -n scenario1 scenario1`
