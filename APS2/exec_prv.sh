#!/bin/bash
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 <option>"
    exit 1
fi
option=$1  # Option (e.g., "eval")
# List of sample files (no extension or with extension, depending on your needs)
samples=("SamplesAps0" "SamplesAps1" "SamplesAps1a")

# Loop through each sample and execute it
for sample in "${samples[@]}"; do
    echo
    echo
    echo "******************************"
    echo "******************************"
    echo
    echo "Running $sample..."
    ./exec.sh "$sample" "$option"
    echo
    echo "******************************"
    echo "******************************"
    echo
done
