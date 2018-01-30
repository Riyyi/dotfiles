#!/bin/sh

if [ "$1" = "kill" ]; then
    kill -9 $(pgrep latex.sh)
else
    DIR="$1"
    OUT="$DIR/out"
    mkdir -p $OUT
    HASH="$(md5sum $DIR/main.tex)"

    while true; do
        sleep 1

        TMP="$(md5sum $DIR/main.tex)"
        if [ "$HASH" != "$TMP" ]; then
            HASH="$TMP"
            pdflatex -no-file-line-error -interaction=nonstopmode -synctex=1 \
            -output-format=pdf -output-directory=$OUT $DIR/main.tex \
            > /dev/null 2>&1
        fi
    done
fi

