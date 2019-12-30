#!/bin/sh

start() {
	[ -z "$1" ] && return 1

	[ -z "$(pidof -x "$(echo "$@" | cut -d ' ' -f 1)")" ] && $@ &
}

start firefox
start urxvt

# Thunar client doesnt get a new pid when running in daemon mode
[ "$(xdo id -a Thunar | wc -l)" -lt 3 ] && thunar &
