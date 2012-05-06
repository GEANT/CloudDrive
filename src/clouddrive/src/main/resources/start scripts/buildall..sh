#!/bin/bash
source ~/.bash_profile
clear
rm -rf build/*
fsc -explaintypes -d build config/*.scala
fsc -explaintypes -d build exceptional/*.scala
fsc -explaintypes -d build utils/*.scala
fsc -explaintypes -d build clouddrive/httpsupport/*.scala
fsc -explaintypes -d build control/*.scala
fsc -explaintypes -d build providers/*.scala
fsc -explaintypes -d build providers/AWS/AWS.scala
fsc -explaintypes -d build providers/AWS/S3.scala
fsc -explaintypes -d build providers/AWS/S3FileSystem.scala
fsc -explaintypes -d build providers/AWS/AWSFileSystem.scala
fsc -explaintypes -d build providers/Filesystem/*.scala
fsc -explaintypes -d build pipes/*.scala
fsc -explaintypes -d build pipes/webdavcmds/*.scala
fsc -explaintypes -d build clouddrive/*.scala
fsc -explaintypes -d build setup/*.scala
fsc -explaintypes -d build *.scala
fsc -explaintypes -d build experiments/*.scala
fsc -explaintypes -d build client/*.scala
cp config.txt build/config.txt
