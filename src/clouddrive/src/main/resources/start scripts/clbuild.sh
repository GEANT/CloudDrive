#!/bin/bash
source ~/.bash_profile
clear
#rm -rf build/*
fsc -make:changed -explaintypes -d build config/*.scala
fsc -explaintypes -d build exceptional/*.scala
fsc -make:changed -explaintypes -d build utils/*.scala
fsc -make:changed -explaintypes -d build clouddrive/httpsupport/*.scala
fsc -make:changed -explaintypes -d build control/*.scala
fsc -make:changed -explaintypes -d build providers/*.scala
fsc -make:changed -explaintypes -d build providers/AWS/AWS.scala
fsc -make:changed -explaintypes -d build providers/AWS/S3.scala
fsc -explaintypes -d build providers/AWS/S3FileSystem.scala
fsc -explaintypes -d build providers/AWS/AWSFileSystem.scala
fsc -make:changed -explaintypes -d build providers/FileSystem/*.scala
fsc -make:changed -explaintypes -d build pipes/*.scala
fsc -make:changed -explaintypes -d build pipes/webdavcmds/*.scala
fsc -make:changed -explaintypes -d build clouddrive/*.scala
fsc -make:changed -explaintypes -d build setup/*.scala
fsc -make:changed -explaintypes -d build *.scala
fsc -make:changed -explaintypes -d build experiments/*.scala
cp clouddrive/config.txt build/config.txt
