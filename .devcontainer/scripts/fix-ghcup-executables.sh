#!/bin/bash

# Script to transform absolute paths containing '.ghcup' or '.stack' to use $GHCHOME variable

# Check if GHCHOME is set
if [ -z "$GHCHOME" ]; then
    echo "Error: GHCHOME environment variable is not set" >&2
    exit 1
fi

# Check if a path argument was provided
if [ $# -eq 0 ]; then
    echo "Usage: $0 <directory_path>" >&2
    exit 1
fi

# Starting directory
START_DIR="$1"

# Check if the directory exists
if [ ! -d "$START_DIR" ]; then
    echo "Error: Directory '$START_DIR' does not exist" >&2
    exit 1
fi

# Function to process a single file
process_file() {
    local file="$1"
    
    # Check if file is executable
    if [ ! -x "$file" ]; then
        return
    fi
    
    # Check if it's a shell script by reading the shebang
    if ! head -n 1 "$file" | grep -q '^#!/bin/sh\|^#!/usr/bin/env sh'; then
        return
    fi
    
    # Check if the file contains '.ghcup' or '.stack' paths
    if ! grep -q '\.\(ghcup\|stack\)' "$file"; then
        return
    fi
    
    echo "Processing: $file"
    
    # Create a temporary file
    temp_file=$(mktemp)
    
    # Process the file line by line
    while IFS= read -r line; do
        # Check if the line contains an absolute path with '.ghcup' or '.stack'
        if echo "$line" | grep -q '\.\(ghcup\|stack\)'; then
            # Extract and replace paths - handle both quoted and unquoted paths
            # This handles lines like: exedir="/stratis/home/tpasch/dev/.ghcup/ghc/9.8.4/lib/ghc-9.8.4/bin"
            # and: exedir="/stratis/home/tpasch/dev/.stack/programs/x86_64-linux/ghc-9.8.4/bin"
            modified_line=$(echo "$line" | sed 's|"\([^"]*\)/\.ghcup\([^"]*\)"|"$GHCHOME/.ghcup\2"|g; s|"\([^"]*\)/\.stack\([^"]*\)"|"$GHCHOME/.stack\2"|g')
            echo "$modified_line" >> "$temp_file"
        else
            echo "$line" >> "$temp_file"
        fi
    done < "$file"
    
    # Replace the original file with the modified one
    mv "$temp_file" "$file"
    
    # Ensure the file remains executable
    chmod +x "$file"
    
    echo "  ✓ Transformed successfully"
}

# Find all 'bin' directories and process executable files within them
# Using -P to not follow symbolic links
find -P "$START_DIR" -type d -name "bin" 2>/dev/null | while read -r bin_dir; do
    # Find all regular files in the bin directory (not following symlinks)
    find "$bin_dir" -maxdepth 1 -type f 2>/dev/null | while read -r file; do
        process_file "$file"
    done
done

# If the START_DIR itself contains executable files, process them directly
if [ -d "$START_DIR" ]; then
    find "$START_DIR" -maxdepth 1 -type f 2>/dev/null | while read -r file; do
        process_file "$file"
    done
fi

echo "Transformation complete!"

