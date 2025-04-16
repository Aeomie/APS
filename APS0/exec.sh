#!/bin/bash

# Ensure folder and option variables are passed
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <folder> <option>"
    exit 1
fi

folder=$1  # Folder path
option=$2  # Option (e.g., "eval")


# Process files based on option
if [ "$option" == "evaluator" ]; then
    echo "******* Evalauteur START *******"
    echo
    for dir in "$folder"/*; do
        if [ -d "$dir" ]; then
            echo
            echo "*****>>> Entering folder: $dir*****"
            echo
            for file in "$dir"/*.aps; do
                [ -e "$file" ] || continue  # Skip if no .aps files
                echo "filename : $file"
                echo "Result: "
                ./evaluator "$file"
                echo "----------------------------------------------"
            done
            echo "*****>>> Done with folder: $dir*****"
            echo
        fi
    done

    # Aps files that are there and not in dirs
    for file in "$folder"/*.aps; do
        [ -e "$file" ] || continue
        echo "filename : $file"
        echo "Result:"
        ./evaluator "$file"
        echo "----------------------------------------------"
    done
    echo "******* Evalauteur END *******"
fi


if [ "$option" == "typeur" ]; then
    echo "******* TYPEUR START *******"
    echo
    for dir in "$folder"/*; do
        if [ -d "$dir" ]; then
            echo "*****>>> Entering folder: $dir*****"
            for file in "$dir"/*.aps; do
                [ -e "$file" ] || continue  # Skip if no .aps files
                echo "filename : $file"
                ./prologTerm "$file" | swipl "typeur.pl"
                echo "----------------------------------------------"
            done
            echo "*****>>> Done with folder: $dir*****"
            echo
        fi
    done

    # Aps files that are there and not in dirs
    for file in "$folder"/*.aps; do
        [ -e "$file" ] || continue
        echo "filename : $file"
        ./prologTerm "$file" | swipl "typeur.pl"
        echo "----------------------------------------------"
    done
    echo "******* TYPEUR END *******"
fi


if [ "$option" == "prologTerm" ]; then

    echo "******* PrologTerm START *******"
    echo
    for dir in "$folder"/*; do
        if [ -d "$dir" ]; then
            echo "*****>>> Entering folder: $dir*****"
            for file in "$dir"/*.aps; do
                [ -e "$file" ] || continue  # Skip if no .aps files
                echo "filename : $file"
                echo "Result: "
                ./prologTerm "$file"
                echo "----------------------------------------------"
            done
            echo "*****>>> Done with folder: $dir*****"
            echo
        fi
    done

    # Aps files that are there and not in dirs
    for file in "$folder"/*.aps; do
        [ -e "$file" ] || continue
        echo "filename : $file"
        echo "Result:"
        ./prologTerm "$file"
        echo "----------------------------------------------"
    done	
    echo "******* PrologTerm END *******"
fi