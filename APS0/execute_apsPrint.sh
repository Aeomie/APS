#!/bin/bash

# Ensure folder variable is passed or defined
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <folder>"
    exit 1
fi

folder=$1  # Accept folder path as an argument

# Collect all .aps files in the given folder
files=("$folder"/*.aps)

# Check if any .aps files are found
if [ ${#files[@]} -eq 0 ]; then
    echo "No .aps files found in $folder."
    exit 1
fi

# Process each .aps file
for file in "${files[@]}"; do
    ./apsPrinter "$file"
done
