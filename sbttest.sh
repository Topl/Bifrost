#! /bin/bash
sbt "test" 2>&1 | tee sbttest.log && sed -i -e "s/\(\|\[0m\|\[0m\[\|\[31m\)//g" sbttest.log
