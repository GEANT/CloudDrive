#!/bin/sh

echo "Ending the preview process, hard"

kill -9 $(cat preview.pid)
rm preview.pid


