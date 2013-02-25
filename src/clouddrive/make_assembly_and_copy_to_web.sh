#!/bin/sh

# Makes the SBT assembly and copies the gigantic JAR as dependency to the web project
# Note that this script has a lot of versions hard-coded.

sbt assembly && \
cp target/scala_2.9.1/SBT\ Clouddrive-assembly-0.7.jar ../web_clouddrive/lib/clouddrive.jar && \
ls  -al ../web_clouddrive/lib/