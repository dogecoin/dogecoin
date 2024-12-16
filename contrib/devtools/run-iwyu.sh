#!/bin/bash

# Run include-what-you-use over the codebase

# Ensure iwyu_tool.py is available
if ! command -v iwyu_tool.py &> /dev/null
then
    echo "iwyu_tool.py could not be found"
    exit 1
fi

# Run iwyu_tool.py with appropriate flags
python3 iwyu_tool.py src -p . -j6 -- -Xiwyu --cxx17ns > iwyu_output.log

# Check if the output file was created
if [ ! -f iwyu_output.log ]; then
    echo "Failed to create iwyu_output.log"
    exit 1
fi

echo "include-what-you-use completed. Output saved to iwyu_output.log"
