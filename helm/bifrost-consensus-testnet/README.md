# Usage
1. Copy file `override-example.yaml` into `override.yaml`.  Update values accordingly.
1. Install the helm chart: `helm install -f /path/to/override.yaml --create-namespace --namespace scenario1 scenario1 ./bifrost-consensus-testnet`
   1. To capture output from the orchestrator: `(DEMO_NAME=your-demo-name; helm upgrade --install -f ./helm/bifrost-consensus-testnet/override-example.yaml --namespace $DEMO_NAME --create-namespace $DEMO_NAME ./helm/bifrost-consensus-testnet/ &&  kubectl wait --timeout=-1s --for=condition=Ready pod/$DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME && kubectl logs --follow $DEMO_NAME-bifrost-consensus-testnet-orchestrator -n $DEMO_NAME)`
2. Observe the logs of each node the Kubernetes cluster in the `scenario1` namespace
3. Terminate the scenario using  `helm delete -n scenario1 scenario1`
