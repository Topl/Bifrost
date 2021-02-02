Project-Gjallarhorn:<br/>A Wallet Application for Bifrost
====================================================================================================================================================================================

Project Gjallarhorn is the wallet application for Bifrost, a Scala client for Topl's blockchain protocol.
Its purpose is mainly to:
- hold and maintain a user's keys
- allow a user to create, sign, and broadcast transactions to Bifrost
- hold and maitain the wallet boxes associated with the user's keys


Install and Build
-------------------
###Dependencies
- Java GraalVM (to install: https://github.com/Topl/Bifrost/wiki/Install-and-Build#2-install-graalvm)
- SBT (to install: https://github.com/Topl/Bifrost/wiki/Install-and-Build#3-install-sbt)

### Build from source code
1. Clone the repository 
   
   ` git clone https://github.com/Topl/Bifrost.git`

2. Go to the project directory

    `cd Bifrost`

Now you must open two terminal windows in order to run bifrost and gjallarhorn at the same time.

First get bifrost running with optional arguments:

   `sbt run --[network] --seed [seed-string] -f`
- network can be: "local", "toplnet", "valhalla", or "private"
- optional seed argument if you want to run bifrost with specific keys
- `-f` flag makes sure to start forging when running

Now that bifrost is running, you can start up the wallet application Gjallarhorn:

In the second window terminal, make sure you're in the correct project directory: `cd Bifrost`

1. start up sbt: simply type: `sbt`
2. navigate to the gjallarhorn project: type `project gjallarhorn`
3. Now the terminal line should say: "sbt:gjallarhorn>". Finally, type `run`

