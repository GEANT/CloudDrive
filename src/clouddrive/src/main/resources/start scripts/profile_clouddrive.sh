#!/bin/bash
cd build
JAVA_OPTS="-agentpath:/Users/maartenkoopmans/localsoft/YourKit_Java_Profiler_9.0.7.app/bin/mac/libyjpagent.jnilib -Xss512k -Xms512M -Xmx512m" scala MainServerStart
