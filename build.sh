#!/bin/bash
# ================================================================
# build.sh — ELIZA: The Session
# Produces a self-contained binary in the same directory as this script.
# Usage:  ./build.sh
# ================================================================

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OUTPUT="$SCRIPT_DIR/eliza"
LISP="$SCRIPT_DIR/eliza.lisp"

echo "Building ELIZA: The Session..."
echo "  Source : $LISP"
echo "  Output : $OUTPUT"
echo ""

sbcl --no-sysinit --no-userinit \
     --eval "(push :building *features*)" \
     --load "$LISP" \
     --eval "(sb-ext:save-lisp-and-die \"$OUTPUT\" \
               :toplevel #'eliza-session::run \
               :executable t \
               :purify t)" \
     2>/dev/null

if [ -f "$OUTPUT" ]; then
    chmod +x "$OUTPUT"
    SIZE=$(du -sh "$OUTPUT" | cut -f1)
    echo "Done!  Binary: $OUTPUT  ($SIZE)"
    echo "Run with:  $OUTPUT"
else
    echo "Build failed. Make sure sbcl is installed and eliza.lisp is present."
    exit 1
fi
