#!/bin/sh

# Kill existing panel
pkill --exact --signal 9 waybar

# Start panel
setsid -f waybar
