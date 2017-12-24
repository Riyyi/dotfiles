#!/bin/bash

xset s 30 30

icon="$HOME/Pictures/lock.png"
tmpbg='/tmp/screen.png'

scrot "$tmpbg"
convert "$tmpbg" -scale 10% -scale 1000% "$tmpbg"
convert "$tmpbg" "$icon" -gravity center -composite -matte "$tmpbg"
i3lock --nofork -u -i "$tmpbg"; xset s off
