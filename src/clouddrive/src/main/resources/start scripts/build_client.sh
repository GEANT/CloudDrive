#!/bin/bash
source ~/.bash_profile
clear
scalac -explaintypes -d build client/*.scala
javac -d build client/*.java
