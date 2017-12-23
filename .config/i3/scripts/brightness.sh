#!/bin/bash

case $BLOCK_BUTTON in
	# right click
	3) xbacklight -set 30;;
	
	# scroll up
	4) xbacklight -inc 10 ;;
	
	# scroll down
	5) xbacklight -dec 10 ;;
esac

printf "%.0f" "$(xbacklight -get)"
