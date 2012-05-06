#!/bin/bash
source ~/.bash_profile
clear
#rm -rf build/*
#TBD : add -optimise flag for production build
fsc -make:changed -explaintypes -d build providers/*.scala
fsc -make:changed -explaintypes -d build pipes/*.scala
fsc -make:changed -explaintypes -d build pipes/webdavcmds/*.scala
