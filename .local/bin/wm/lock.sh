#!/bin/sh

LOCK="$XDG_CACHE_HOME/lock.png"

# Cache the lock image
if [ ! -f "$LOCK" ]; then
	touch "$LOCK"

	# Get the resolution of the primary monitor
	RES="$(xrandr -q | grep ' connected primary' \
		| grep -Eo '[0-9]{3,4}x[0-9]{3,4}' | head -c -1)"

	# Generate lock image
	convert "$(dirname "$0")/lock.png" -background none -gravity center -extent "$RES" "$LOCK"
fi

# Disable screensaver
revert() {
	xset s off
	xset -dpms
}

# Set screensaver to 30 seconds
trap revert HUP INT TERM
xset +dpms dpms 30 30 30

i3lock -n -i "$LOCK" -B 6 -S 1 -e \
	--radius=47 \
	--ring-width=5.0 \
	--verifcolor=00000000 \
	--wrongcolor=00000000 \
	--timecolor=00000000 \
	--datecolor=00000000 \
	--layoutcolor=00000000 \
	--insidecolor=00000000 \
	--insidevercolor=00000000 --insidewrongcolor=00000000 \
	--ringcolor=00000000 \
	--ringvercolor=FFFFFFC0 --ringwrongcolor=DA2825C0 \
	--keyhlcolor=FFFFFFC0 --bshlcolor=DA2825C0 \
	--linecolor=00000000 --separatorcolor=00000000 \
	-k \
	--timesize=64 --datesize=24 \
	--timestr="%I:%M %p" --datestr="%A, %B %e" \
	--timepos="ix:iy-250" --datepos="ix:iy-200" \
	--timecolor=FFFFFFC0 --datecolor=FFFFFFC0

revert
