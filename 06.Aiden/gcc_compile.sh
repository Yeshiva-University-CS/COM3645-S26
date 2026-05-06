#!/bin/bash
set -e

if [[ "$OSTYPE" == "msys" || "$OSTYPE" == "cygwin" || "$OS" == "Windows_NT" ]]; then
    echo "Error: this script must be run inside WSL, not native Windows."
    exit 1
fi

JAR="target/Aiden-1.jar"
INPUT="$1"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <file.aiden> [output_name]"
    exit 1
fi

OUTPUT="${2:-$(basename "$INPUT" .aiden)}"
ASM_FILE="/tmp/${OUTPUT}.s"

echo "Compiling $INPUT..."
java -jar "$JAR" -compile x86 "$INPUT" > "$ASM_FILE"

echo "Linking..."
gcc -o "$OUTPUT" "$ASM_FILE"

echo "Done: ./$OUTPUT"
