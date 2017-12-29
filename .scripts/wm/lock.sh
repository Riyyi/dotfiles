#!/bin/bash

ICON="$HOME/Pictures/lock.png"
TMPBG='/tmp/screen.png'

revert() {
	xset dpms 0 0 0
}

trap revert HUP INT TERM
xset +dpms dpms 30 30 30

scrot "$TMPBG"
convert "$TMPBG" -scale 10% -scale 1000% "$TMPBG"
convert "$TMPBG" "$ICON" -gravity center -composite -matte "$TMPBG"

i3lock -n -i "$TMPBG" -e \
	--textcolor=00000000 \
	--insidecolor=00000000 \
	--insidevercolor=00000000 --insidewrongcolor=00000000 \
	--ringcolor=00000000 \
	--ringvercolor=FFFFFFC0 --ringwrongcolor=DA2825C0 \
	--keyhlcolor=FFFFFFC0 --bshlcolor=DA2825C0 \
	--linecolor=00000000 --separatorcolor=00000000 \
	-k \
	--timesize=64 --datesize=24 \
	--timestr="%I:%M %p" --datestr="%A, %B %e" \
	--timepos="ix-200:iy-250" --datepos="ix-200:iy-210" \
	--timecolor=FFFFFFC0 --datecolor=FFFFFFC0

revert
