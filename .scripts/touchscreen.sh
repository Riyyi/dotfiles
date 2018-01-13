#!/bin/sh

device="04F3200A:00 04F3:2373"
state=$(xinput --list-props "$device" | grep "Device Enabled" | sed -nr 's/.*:\t([0-9])/\1/p')

if [ "$state" = "0" ]; then
	xinput set-prop "$device" 140 1
else
	xinput set-prop "$device" 140 0
fi

