#!/bin/sh
<<COMMENT
This script is used to pipelining all executions on files stored under "./output".

The pipline: 
    1. Run the Bitmlx compiler
    2. Replace all hashes with python script
    3. Compile each modified contract and store result in corresponding .blz files

Usage: 
    ./BitMLx_pipeline.sh
COMMENT

stack run || {
    echo "Error: stack run failed."
    exit 1
}

file_ext=".rkt"
result_ext=".blz"
racket_dir_path="./output"

for file_path in $racket_dir_path/*$file_ext
do
    result_path="${file_path%${file_ext}}${result_ext}"
    python3 replace_hash.py ${file_path}
    racket ${file_path} > ${result_path}
done
