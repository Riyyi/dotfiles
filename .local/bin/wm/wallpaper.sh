#!/bin/sh

WALLPAPER="$HOME/pictures/wallpaper.jpg"

# Overwrite wallpaper with new image if provided
[ -n "$1" ] && cp "$1" "$WALLPAPER"

# Set wallpaper
if [ "$WAYLAND" = true ]; then
	pkill hyprpaper
	setsid -f hyprpaper

	# Wait until hyprpaper is available
	while [ -z "$(find "$XDG_RUNTIME_DIR/hypr" -name ".hyprpaper.sock")" ]; do
		sleep 0.1
	done
	sleep 0.1

	hyprctl hyprpaper preload "$WALLPAPER"
	hyprctl hyprpaper wallpaper ",$WALLPAPER"
else
	feh --no-fehbg --bg-fill "$WALLPAPER"
fi
