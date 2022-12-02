# Topl Grpc

Topl Google Remote Procedure Call (toplGrpc) is a subproject of Bifrost

It uses [fs2-grpc](https://github.com/typelevel/fs2-grpc)plugin, a gRPC implementation for FS2/cats-effect 

Protobuf files reflected in the directory <topl-grpc/src/main/protobuf>, they belong to other repository, and they are integrated using the git-submodules tool

- Files Repository [protobuf-specs](https://github.com/Topl/protobuf-specs)
  - Git-Tools-Submodules [Submodules](https://git-scm.com/book/en/v2/Git-Tools-Submodules)
  proj

### Scala Generation
fs2-grpc takes protobuf folder and generate scala code from them

> sbt:topl-grpc> clean
[success] Total time: 0 s, completed Dec 2, 2022, 5:22:40 PM
> sbt:topl-grpc> compile

Compiling 14 protobuf files to 
- Bifrost/topl-grpc/target/scala-2.13/src_managed/main/scalapb    << (Models)
- Bifrost/topl-grpc/target/scala-2.13/src_managed/main/fs2-grpc   << (Services)


### Useful commands

Adding a submodule, (we don't need, it was done)
> git submodule add -f git@github.com:Topl/protobuf-specs.git ./topl-grpc/src/main/protobuf

Shows the status of existing submodules
> git submodule                                                                            
> 92304ffb47ee8ce3b8a1498b8e1fd3896819319c topl-grpc/src/main/protobuf (heads/main)

Pull
> git submodule foreach 'git pull'

checkout a branch in submodule
> git submodule foreach 'git checkout main'