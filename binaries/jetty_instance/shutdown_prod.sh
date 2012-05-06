#!/bin/sh

echo "Ending the production process, hard"

kill -9 $(cat prod.pid)
rm prod.pid


