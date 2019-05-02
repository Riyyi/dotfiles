#!/bin/sh

# Enable mathematics in POSIX shell
calc() { awk "BEGIN{print $*}"; }

# Calculate size
MON_WIDTH=$(xdotool getdisplaygeometry | awk '{print $1}')
MON_HEIGHT=$(xdotool getdisplaygeometry | awk '{print $2}')
NEW_WIDTH=$(calc "$MON_WIDTH * 0.2")
NEW_HEIGHT=$(calc "$NEW_WIDTH / 16 * 9")

# Calculate position
BORDER=23
X=$(calc "$MON_WIDTH - $NEW_WIDTH - $BORDER")
Y=$(calc "$MON_HEIGHT - $NEW_HEIGHT - $BORDER")

# Set window
CURRENT=$(xdotool getwindowfocus)
xdotool windowsize "$CURRENT" "$NEW_WIDTH" "$NEW_HEIGHT"
xdotool windowmove "$CURRENT" "$X" "$Y"
