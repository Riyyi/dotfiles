#!/bin/sh

emacs() {
	# Create new frame if (there isnt one || no file specified)
	if [ -z "$(pgrep emacsclient)" ] || [ "$1" = "" ]; then
		emacsclient -a '' -c "$@" > /dev/null 2>&1 &
	else
		emacsclient -a '' "$@" > /dev/null 2>&1 &
	fi
}

depend() {
	pactree -u -d 1 "$1" | tail -n +2 | sort
}

java_doc() {
	mkdir -p './doc';
	javadoc -d './doc' -- *.java
}

java_run () {
	mkdir -p './out';
	javac -d './out' "$1.java";
	java -cp './out' "$1"
}

raspbian() {
	sudo systemctl start avahi-daemon.service
	if ! ip a show usb0 | grep -q 'inet6'; then
		sudo dhcpcd usb0
	fi
	ssh -6 pi@"$(avahi-resolve-host-name raspberrypi.local | awk '{ print $2 }')"%usb0
	sudo systemctl stop avahi-daemon.service
}

# $1 = find, $2 = list, $3 = list separator
in_list() {
	[ -z "$1" ] || [ -z "$2" ] && return 1
	if ! echo "$2" | awk -v m="^$1$" -v RS="${3:-' '}" '$0 ~ m { exit 1 }'; then
		return 0
	else
		return 1
	fi
}

pastebin() {
	echo "$1" | curl -F 'f:1=<-' ix.io
}

stream() {
	[ -z "$2" ] && QUALITY="720p" || QUALITY="$2"

	streamlink --player mpv "https://twitch.tv/$1" "$QUALITY" > /dev/null 2>&1 &
}

"$@"
