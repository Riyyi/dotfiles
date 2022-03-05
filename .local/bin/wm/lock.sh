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

# >>> hostname=arch-desktop
radius=92
ringWidth="10.0"
# <<<
# >>> hostname=arch-laptop
# radius=46
# ringWidth="5.0"
# <<<

i3lock -n -i "$LOCK" -B 6 -S 1 -e \
	--radius="$radius" \
	--ring-width="$ringWidth" \
	--verif-color=00000000 \
	--wrong-color=00000000 \
	--time-color=00000000 \
	--date-color=00000000 \
	--layout-color=00000000 \
	--inside-color=00000000 \
	--insidever-color=00000000 --insidewrong-color=00000000 \
	--ring-color=00000000 \
	--ringver-color=FFFFFFC0 --ringwrong-color=DA2825C0 \
	--keyhl-color=FFFFFFC0 --bshl-color=DA2825C0 \
	--line-color=00000000 --separator-color=00000000 \
	-k \
	--time-size=64 --date-size=24 \
	--time-str="%I:%M %p" --date-str="%A, %B %e" \
	--time-pos="ix:iy-250" --date-pos="ix:iy-200" \
	--time-color=FFFFFFC0 --date-color=FFFFFFC0

revert
