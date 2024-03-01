#!/bin/bash
# Runs all the processed racket files (with tests) to see if they still run after being processed
if [ $# -ne 1 ]; then
    echo "Usage: ./sanity_check_processed.sh <path to processed files>"
    exit 1
fi

for file in $1/*/*.rkt; do
    echo "+ Running $file"
    racket $file
    if [ $? -ne 0 ]; then
        echo "- ERROR RUNNING $file"
    fi
done
