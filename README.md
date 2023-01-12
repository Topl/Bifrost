Project Bifrost:<br/>A Scala Client for the Topl Blockchain Protocol 
====================================================================================================================================================================================
Project Bifrost is a Scala based reference implementation of the Topl blockchain tokenization protocol. 
The Topl protocol is conceived as an opinionated blockchain framework developed to support tokenization and promote circular and impact-aware markets and economies. 
Applications of the network include:
- Supply chain tracking and verification for ethically and sustainably produced agriculture and mineral products;
- Interoperable carbon markets for nature-based and technology driven standards;
- Inclusive finance and trade finance projects, especially in the Global South

Releases
----------
Begin interacting with Topl protocol using the latest `bifrost-node` client available under [Releases](https://github.com/Topl/Project-Bifrost/releases/latest). 
An alternative host for each released JAR is accessible at `https://repo.topl.network/jars/<X.X.X>/bifrost-node-X.X.X.jar` where `X.X.X` is a tagged software version. 
Additioanlly, build artifacts for each tagged version are also available at [repo.topl.network](https://repo.topl.network). 
An MD5 checksum is provideded for each JAR artifact at this location under `bifrost-node-X.X.X.jar.md5`

Docker containers for each release are available via [Github Container Repository](https://github.com/Topl/Bifrost/pkgs/container/bifrost-node) and [Docker Hub](https://hub.docker.com/r/toplprotocol/bifrost-node/tags)

This source-code repository is maintained as a monorepo for Scala projects related to the Topl protocol.
Therefore, the source code for several published libraries of independent utility outside `bifrost-node` are maintained in this repository and build artifacts are published to [Maven](https://mvnrepository.com/artifact/co.topl)

#### Application Versioning
Bifrost applies [semantic versioning](https://semver.org/) with respect to the protocol consensus mechanism.
This is chosen since blockchains must maintain strict backwards binary compatibility with all previous consensus, application, and network messages to allow trustless bootstrapping of new network participants from genesis. 
We apply the following rules:
- `MAJOR` index is conditioned on hard-forks and substantial network upgrades requiring significant community coordination
- `MINOR` index is incremented on soft-forks and may include breaking API changes (to any module within the Bifrost monorepo) or significant feature improvements being newly introduced 
- `PATCH` index is used for non-breaking changes and bug fixes

Development
-------------------
Developers can reference the [Development Setup](./docs/DevelopmentSetup.md) guide for the setup procedure.

Installation
-------------------
Check out our [Installation instructions](https://github.com/Topl/Bifrost/wiki/Install-and-Build) for step-by-step
instructions using a JAR, a Docker container, or from source. **Do not attempt to build this in a
windows environment. Some of the tests will fail under windows.**

Docs
----------
Additional documentation, including tutorials and code recipes, can be found at [docs.topl.co](http://docs.topl.co) 

Wiki
----------
The latest version of the Topl wiki (our in-depth alternative to a traditional whitepaper) can be found at [wiki.topl.co](https://wiki.topl.co).

Command Line Reference
----------
This output is generated using `sbt node/run --help` 
```
-c --config <str>       file path to a user defined config file
-d --debug              Turn on debugging information
-n --network <network>  specify preset network by name
-s --seed <str>         String used to deterministically generate addresses during startup
-f --forge              Enable forging as soon as the node starts
--disableAuth           Allow the node to receive API requests (via JSON-RPC) without an API key
--apiKeyHash <str>      If API key protection is enabled, this argument specifies the Blake2b256
                        hash of API key required by the JSON-RPC server
```

Testing
-------
**NOTE:** These instructions assume the source code, sbt, JDK 11, and Docker have been previously installed.
- To run unit tests
   1. Go to the project directory: `cd Bifrost`
   1. Type: `sbt test`
      - **NOTE 1**: Using sbt to run tests using the Windows command line may error. Either use Intellij IDEA's test runner or run sbt in a Unix environment. Alternatively, if you have the Linux Subsystem for Windows enabled, you can just use bash.
      - **NOTE 2**: In some systems, the tests might fail with an `java.lang.OutOfMemoryError`. The solution to this problem is to give the JVM more heap space and sometimes stack space. This is done through the JVM parameters `-Xmx` and `-Xms`. These parameters are passed to sbt through the file `.jvmopts`. For example, if you want to give you JVM `4G` of heap space and `1G` of stack space, you need to create a file `.jvmopts` containing the following line: `-Xmx4G -Xms1G`. With this file in the project's root directory sbt will run with the right heap and stack size.
- To publish a Docker image for local testing, type: 
   ```
   sbt node/docker:publishLocal
   ```
   - To run the published container, type: `docker run bifrost:x.x.x` (where `x.x.x` is the version that was published).
   - To pass command line arguments, type `docker run bifrost:x.x.x -s mySpecialSeed`
- To run integration tests:
   1. Install Docker
   1. Build, Run, and Cleanup the Integration Test via Docker:
      - On Unix systems, run:
      ```
      IMAGE_ID=$(docker build -q -f node/src/it/resources/Dockerfile .) && docker run --rm -v /var/run/docker.sock:/var/run/docker.sock $IMAGE_ID && docker image rm -f $IMAGE_ID
      ```
      - On Windows systems, using PowerShell, run: 
      ```
      `for /f "tokens=* USEBACKQ" %i in (`docker build -q -f node/src/it/resources/Dockerfile .`) do (set IMAGE_ID=%i) && docker run --rm -v //var/run/docker.sock:/var/run/docker.sock %IMAGE_ID% && docker image rm -f %IMAGE_ID%`
      ```
   - NOTE: 
      - You may not see any output for quite a while.  The Docker image takes a long time to build, and its output is mostly silent.  You will only see output for the run of the integration test.
      - To speed up repeated runs, remove the cleanup step (`&& docker image rm -f $IMAGE_ID`) from the end of the command.
      - When local testing through an IDE instead of through the Dockerfile, it is recommended to create a `version.sbt` at the project root with contents `ThisBuild / version := "it"`.   This file should not be checked into git, but it will help keep the published Docker tag stable.

Contributions
-------------
We appreciate your interest in this project and welcome contributions!

If you'd like to contribute to Project Bifrost, please fork, fix, commit and send a pull request so that we can review your code and merge it when it is complete. For more complex changes, please contact us via Gitter or Slack to ensure those changes are reasonable and/or get some early feedback to expedite the review and merge process.

**Please read our [Contributing Guide](https://github.com/Topl/Bifrost/blob/main/.github/CONTRIBUTING.md) for more information before submitting any pull requests, as the submission of any content to Topl's repositories constitutes your understanding of and agreement to our Contributor License Agreement and Code of Conduct.**

To keep up with our development

- View open [issues](https://github.com/Topl/Project-Bifrost/issues)!

License
-------
Project Bifrost is licensed under the
[Mozilla Public License 2.0 (MPL 2.0)](https://opensource.org/licenses/MPL-2.0), also included
in our repository in the `LICENSE` file.
