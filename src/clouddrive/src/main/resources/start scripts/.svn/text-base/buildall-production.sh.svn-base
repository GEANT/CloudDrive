#!/bin/bash
source ~/.bash_profile
clear
rm -rf build/*
scalac -optimise -d build config/*.scala
scalac -optimise -d build exceptional/*.scala
scalac -optimise -d build utils/*.scala
scalac -optimise -d build clouddrive/httpsupport/*.scala
scalac -optimise -d build control/*.scala
scalac -optimise -d build providers/*.scala
scalac -optimise -d build providers/AWS/AWS.scala
scalac -optimise -d build providers/AWS/S3.scala
scalac -optimise -d build providers/AWS/S3FileSystem.scala
scalac -optimise -d build providers/AWS/AWSFileSystem.scala
scalac -optimise -d build providers/Filesystem/*.scala
scalac -optimise -d build pipes/*.scala
scalac -optimise -d build pipes/webdavcmds/*.scala
scalac -optimise -d build clouddrive/*.scala
scalac -optimise -d build setup/*.scala
scalac -optimise -d build *.scala
cp prod_config.txt build/config.txt
