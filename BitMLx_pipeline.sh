#!/bin/sh
<<COMMENT
This script is used for pipelining all executions on files stored under "./output".

The pipline: 
    1. Run the Bitmlx compiler
    2. Replace all hash placeholders with actual hashes
    3. Compile each modified contract and store result in corresponding .balzac files

Usage: 
    ./BitMLx_pipeline.sh
COMMENT

stack run || {
    echo "Error: BitMLx compiler failed."
    exit 1
}

file_ext=".rkt"
result_ext=".balzac"
racket_dir_path="./output"

for file_path in $racket_dir_path/*$file_ext
do
    result_path="${file_path%${file_ext}}${result_ext}"
    python3 replace_hash.py ${file_path}
    racket ${file_path} > ${result_path}
done
