#!/bin/bash

man() {
	LESS_TERMCAP_mb=$'\e[01;31m' \
	LESS_TERMCAP_md=$'\e[01;34m' \
	LESS_TERMCAP_me=$'\e[0m' \
	LESS_TERMCAP_so=$'\e[00;37m' \
	LESS_TERMCAP_se=$'\e[0m' \
	LESS_TERMCAP_us=$'\e[04;95m' \
	LESS_TERMCAP_ue=$'\e[0m' \
	command man "$@"
}

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
