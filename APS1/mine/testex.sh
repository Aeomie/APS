#!/bin/bash

# Ensure folder and option variables are passed
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <folder> <option>"
    exit 1
fi

folder=$1  # Folder path
option=$2  # Option (e.g., "eval")

# Collect all .aps files in the given folder
files=("$folder"/*.aps)

# Check if any .aps files are found
if [ ${#files[@]} -eq 0 ]; then
    echo "No .aps files found in $folder."
    exit 1
fi

# Process files based on option
if [ "$option" == "eval" ]; then
    for file in "${files[@]}"; do
		echo "filename : $file"
        ./evaluator "$file"
    done
fi
