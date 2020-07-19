#!/bin/sh

DATE="$(date +'%A, %B %d, %Y')"

notify-send -r 3 "Date" "\
$DATE"
