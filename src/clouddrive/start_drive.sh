#!/bin/bash
nohup java -Xmx2G -XX:+UseConcMarkSweepGC -XX:+CMSIncrementalMode -XX:+CMSIncrementalPacing -XX:MaxPermSize=256m -XX:PermSize=256m -jar clouddrive.jar &
