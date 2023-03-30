Bifrost: Topl Network Full Node
====================================================================================================================================================================================
Bifrost is the official Scala-based reference implementation of a full node running the Topl protocol.
The Topl protocol enables a new, more inclusive and sustainable economy through purpose-built blockchain technology.


Releases
----------
Begin interacting with Topl protocol using the latest `bifrost-node` client available under [Releases](https://github.com/Topl/Project-Bifrost/releases/latest).
Docker containers for each release are available via [Github Container Repository](https://github.com/Topl/Bifrost/pkgs/container/bifrost-node) and [Docker Hub](https://hub.docker.com/r/toplprotocol/bifrost-node/tags).
We encourage all production deployments of Bifrost to make use of our Docker releases.

### Release Versioning
Tetra-eon releases of Bifrost are currently versioned on an incrementing alpha version (i.e. `2.0.0-alpha0`, `2.0.0-alpha1`).  Once the implementation reaches a stable point, we will adopt [semantic versioning](https://semver.org/) with respect to the protocol consensus mechanism.
We chose to use semantic versioning because blockchains must maintain strict binary compatibility with all previous consensus, application, and network messages to allow trustless bootstrapping of new network participants from genesis.

We apply the following rules to our MAJOR.MINOR.PATCH versions:
- `MAJOR` index reflects hard-forks or substantial network upgrades requiring significant community coordination
- `MINOR` index captures soft-forks, and may include breaking API changes (to any module within the Bifrost monorepo) or significant new feature introductions
- `PATCH` index indicates non-breaking changes and bug fixes

Getting Started
-------------------
Please read our [Installation instructions](https://github.com/Topl/Bifrost/wiki/Install-and-Build) for a step-by-step
guide to installing and running a Bifrost node. At this time, **we officially support the use of a Debian-based distributions such as Ubuntu only**. While other platforms may function with additional effort, our documentation does not explicitly consider these targets.

Command Line Reference
----------
Bifrost accepts the following command line arguments:
```
  --config <str>              Zero or more config files (.conf, .json, .yaml) to apply to the node. Config files stack such that the last config file takes precedence. To specify an internal resource, prefix the value with "resource://".
  --logbackFile <str>         An optional path to a logback.xml file to override the logging configuration of the node.
  --debug                     An optional flag to enable debug mode on this node.
  --dataDir <str>             The directory to use when saving/reading blockchain data
  --stakingDir <str>          The directory of the block producer's staking keys
  --rpcBindHost <str>         The hostname to bind to for the RPC layer (i.e. localhost or 0.0.0.0)
  --rpcBindPort <int>         The port to bind to for the RPC layer (i.e. 9085)
  --p2pBindHost <str>         The hostname to bind to for the P2P layer (i.e. localhost or 0.0.0.0)
  --p2pBindPort <int>         The port to bind to for the P2P layer (i.e. 9084)
  --knownPeers <str>          A comma-delimited list of host:port values to connect to at launch (i.e. 1.2.3.4:9084,5.6.7.8:9084)
  --testnetTimestamp <long>   A UTC Unix epoch timestamp (ms) to use when seeding a private testnet.
  --testnetStakerCount <int>  The number of stakers to initialize.
  --testnetStakerIndex <int>  The index of the staker to launch.
```
You may also view `sbt "node/run --help"` for more information.


Docs
----------
Additional documentation, tutorials, and code examples can be found at [docs.topl.co](http://docs.topl.co).

Running
-------------------
The easiest way to run a Bifrost node is to use the official Docker image.  To run a Bifrost node using Docker:

`docker run toplprotocol/bifrost-node:2.0.0-alpha2 --rpcBindHost 0.0.0.0`
- `2.0.0-alpha2` can be substituted with the desired version
- We recommend using the `--rpcBindHost 0.0.0.0` argument when running a Docker container to allow host-machine connections to the RPC layer.


Development
-------------------
Developers should reference the [Development Setup](./docs/DevelopmentSetup.md) guide for setting up a new development environment for Bifrost.

Testing
----------
**NOTE:** These instructions assume a working development environment.

To run the Bifrost unit test suite from the project directory: `sbt test`

To publish a Docker image from the project directory for local testing: `sbt node/Docker/publishLocal`.  To use the locally published image, run `docker run toplprotocol/bifrost-node`.

Contributions
-------------
We appreciate your interest in this project and welcome contributions! You can view open issues [here](https://github.com/Topl/Bifrost/issues).

If you'd like to contribute to Bifrost, please submit pull requests so that we can review your code. For more complex changes, please contact us via [Discord](https://discord.gg/SjYVTBnsQR) to ensure changes are reasonable and to receive  early feedback. This will significantly expedite the process.

**Please read our [Contributing Guide](https://github.com/Topl/Bifrost/blob/main/.github/CONTRIBUTING.md) for more information before submitting any pull requests, as the submission of any content to Topl's repositories constitutes your understanding of and agreement to our Contributor License Agreement and Code of Conduct.**

License
-------
Project Bifrost is licensed under the
[Mozilla Public License 2.0 (MPL 2.0)](https://opensource.org/licenses/MPL-2.0).
