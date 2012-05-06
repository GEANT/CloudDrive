#!/bin/sh

echo "Ending the development process, hard"

kill -9 $(cat development.pid)
rm development.pid


