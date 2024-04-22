#!/bin/sh
<<COMMENT
This script is used for pipelining all executions on files stored under "./output".

The pipline: 
    1. Run the Bitmlx compiler.
    2. Replace all hash placeholders with actual hashes.
    3. Compile each modified contract and store result in corresponding .balzac files.
    4. Read statistics from files and generate a table based on them.

Usage: 
    ./BitMLx_pipeline.sh
COMMENT

echo "Started BitMlx pipelining. \nRunning the BitMLx compiler..."
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
    echo "\nReplacing placeholders in $file_path"
    python3 replace_hash.py ${file_path}

    echo "Compiling $file_path"
    racket ${file_path} > ${result_path} || {
        echo "Error: Failed to compile $file_path"
        exit 1
    }
    echo "Successfully compiled $file_path"
done

python3 read_statistics.py 

echo "\nFinished BitMLx pipelining!"
