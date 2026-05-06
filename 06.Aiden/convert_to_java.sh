#!/bin/bash
set -e

JAR="target/Aiden-1.jar"
INPUT="$1"

if [ -z "$INPUT" ]; then
    echo "Usage: $0 <file.aiden>"
    exit 1
fi

if [ ! -f "$INPUT" ]; then
    echo "Error: file not found: $INPUT"
    exit 1
fi

if [ ! -f "$JAR" ]; then
    echo "Error: JAR not found at $JAR — run 'mvn package' first"
    exit 1
fi

# Derive class name from filename: capitalize first letter
BASENAME=$(basename "$INPUT" .aiden)
PROGRAM_NAME="${BASENAME^}"
JAVA_FILE="${PROGRAM_NAME}.java"

echo "Converting $INPUT -> $JAVA_FILE..."
java -jar "$JAR" -convert java "$INPUT" > "$JAVA_FILE"

echo "Compiling $JAVA_FILE..."
javac "$JAVA_FILE"

echo "Running $PROGRAM_NAME..."
java "$PROGRAM_NAME"
