#!/bin/bash

depend() {
	pactree -u -d 1 $1 | tail -n +2
}

java-doc() {
	mkdir -p './doc';
	javadoc -d './doc' *.java
}

java-run() {
	mkdir -p './out';
	javac -d './out' "$1.java";
	java -cp './out' "$1"
}

raspbian() {
	sudo systemctl start avahi-daemon.service
	if ! ip a show usb0 | grep -q 'inet6'; then
		sudo dhcpcd usb0
	fi
	ssh -6 pi@$(avahi-resolve-host-name raspberrypi.local | awk '{ print $2 }')%usb0
	sudo systemctl stop avahi-daemon.service
}

"$@"
