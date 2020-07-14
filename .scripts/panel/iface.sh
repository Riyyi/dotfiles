#!/bin/sh

WIRED="${1:-eth0}"

IP="$(ip a show "$WIRED" \
		| awk '/inet / { print substr($2, 0, length($2) - 3) }')"

notify-send -r 3 "Wired" "\
Interface: $WIRED
IP:        $IP"
