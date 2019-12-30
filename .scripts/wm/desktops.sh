#!/bin/sh

for m in `bspc query -M`; do
	bspc monitor $m -d \
		 "$WS1" "$WS2" "$WS3" "$WS4" "$WS5" \
		 "$WS6" "$WS7" "$WS8" "$WS9" "$WS0"
done
