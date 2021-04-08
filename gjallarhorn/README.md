Project-Gjallarhorn:<br/>A Wallet Application for Bifrost
====================================================================================================================================================================================

Project Gjallarhorn is the wallet application for Bifrost, a Scala client for Topl's blockchain protocol.
Its purpose is mainly to:
- hold and maintain a user's keys
- allow a user to create, sign, and broadcast transactions to Bifrost
- hold and maitain the wallet boxes associated with the user's keys


Install and Build
-------------------
### Dependencies
- Java GraalVM (to install: https://github.com/Topl/Bifrost/wiki/Install-and-Build#2-install-graalvm)
- SBT (to install: https://github.com/Topl/Bifrost/wiki/Install-and-Build#3-install-sbt)

### Build from source code
1. Clone the repository 
   
   ` git clone https://github.com/Topl/Bifrost.git`

2. Go to the project directory

    `cd Bifrost`

3. Run Gjallarhorn 
    
    (Skip this first step if you want to run in offline mode)
    
    i. First start running Bifrost with optional arguments:
    
    `sbt run --[network] --seed [seed-string] -f`
        
    - network can be: "local", "toplnet", "valhalla", or "private"
    - optional seed argument if you want to run bifrost with specific keys
    - `-f` flag: ensures to start forging when running
    
    Example: `sbt run --local --seed test -f`
     
     ii. Now that bifrost is running, you can start up the wallet application, Gjallarhorn:
     - Open up a second terminal window and navigate to the correct project directory: `cd Bifrost`
        1. Start up sbt: type `sbt`
        2. Navigate to the gjallarhorn project: type `project gjallarhorn`
        3. Now the terminal line should say: "sbt:gjallarhorn>". Finally, type `run`
        
 *Note about shut-down:
 
 In order to successfully shutdown gjallarhorn, you must press [ctrl]+[c]. Once the execution has stopped (you should see "sbt:gjallarhorn>"), you must type `exit` in order to terminate the akka actors and fully shut down the application. (There's a ticket for this #876)
              
Important settings for running
-------------------
The settings for gjallarhorn can be found in `src/main/resources/application.conf`
    
   1. chainProvider address:
        - the chain provider must match the hostname and port for Bifrost's akka.remote.classic.netty.tcp config (this can be found in the main Bifrost repo under src/main/resources/application.conf)
        - more specifically, the chain provider should follow this format:
        `[bifrost.network.agentName]@[akka.remote.classic.netty.tcp.hostname]:[akka.remote.classic.netty.tcp.port]`
        - the default should be set to `bifrost-client@127.0.0.1:9087`
   2. two types of chain providers: this refers to how gjallarhorn communicates with Bifrost.
        - "HttpChainProvider": communication will go through api route
        - "AkkaChainProvider": communication will go through akka actors
        
Testing
-------------------
In order to run many of the tests, you must have bifrost running.

If bifrost's running, you can run the tests by typing: `sbt "project gjallarhorn" test`

You can also run individual test files by typing: `sbt "project gjallarhorn" "testOnly <test-filename>"`

*sbt test looks for files within the `test/scala` folder so running a single test should look like: `sbt "project gjallarhorn" "testOnly KeysSpec"`

**Note: right now you must run the tests individually or else you will get an error about the akka routing port being bound multiple times. There are already tickets for this issue (#892 and #895). 

Documentation
-------------------
The documentation for gjallarhorn was created using ScalaDocs (information about ScalaDocs can be found at: https://docs.scala-lang.org/style/scaladoc.html)

In order to load the documentation into your local server, you can follow these steps:
1. Navigate to the bifrost directory `cd Bifrost`
2. run: `sbt "project gjallarhorn" doc`
3. Now you should find an index.html file in `gjallarhorn/target/scala-2.12/api/` which you can usually just open up in your code editor and click on the web browser icon you prefer, 
or you can go to: http://localhost:63342/bifrost/gjallarhorn/target/scala-2.12/api/index.html

The documentation can also be found through GitHub Pages: 
 - There exists a github action (which can be found in `Bifrost/.github/workflows/gjallarhorn_doc_gen.yml`) that should update the documentation page every time there is a new `git push` to the specified branch defined in the .yml file.
 - Your GitHub pages must be set-up within the settings file of the repository 
 
 Frontend
 ---------------
 
 The code for Gjallarhorn's user interface can be found in this repository: https://github.com/Topl/Gjallarhorn-UI
 
 The build files can be found at: `Bifrost/gjallarhorn/src/main/resources/ui/build`
 
 To load the local site 
 - Note: gjallarhorn only accepts requests from a set list of domains which can be found in: `Bifrost/gjallarhorn/src/main/scala/http/HttpService.scala`
 - find the index.html files within the folder specified above and right click on it to open with a web browser.
 - If you use intellij, open the `index.html` file within the `ui/build` folder and there should be a pop-up with web browsers to choose from to load the site
      - You can modify the port by opening up Settings/Preferences -> Build,Execution,Deployment -> Debugger : then scroll down to "Built-in server" to input the correct port.

 
 Make sure project gjallarhorn is running in order to interact with the site (and if you want to be in online mode, make sure bifrost is running too!)


