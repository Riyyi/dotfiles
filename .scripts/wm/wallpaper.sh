#!/bin/sh

# Overwrite wallpaper with new image if provided
[ -n "$1" ] && cp "$1" "$HOME/pictures/wallpaper.jpg"

# Set wallpaper
feh --no-fehbg --bg-fill "$HOME/pictures/wallpaper.jpg"
