# Usage
1. Copy file `override-example.yaml` into `override.yaml`.  Update values accordingly.
1. Install the helm chart: `helm install -f /path/to/override.yaml --create-namespace --namespace scenario1 scenario1 ./bifrost-consensus-testnet`