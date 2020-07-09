#!/bin/sh

depend() {
	pactree -u -d 1 "$1" | tail -n +2 | sort
}

emacs() {
	# Create new frame if (there isnt one || no file specified)
	if [ -z "$(pgrep emacsclient)" ] || [ "$1" = "" ]; then
		emacsclient -a '' -c "$@" > /dev/null 2>&1 &
	else
		emacsclient -a '' "$@" > /dev/null 2>&1 &
	fi
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

java_doc() {
	mkdir -p './doc';
	javadoc -d './doc' -- *.java
}

java_run () {
	mkdir -p './out';
	javac -d './out' "$1.java";
	java -cp './out' "$1"
}

mkcd(){
    mkdir -p "$1" && cd "$1" || exit
}

pastebin() {
	echo "$1" | curl -F 'f:1=<-' ix.io
}

raspbian() {
	sudo systemctl start avahi-daemon.service
	if ! ip a show usb0 | grep -q 'inet6'; then
		sudo dhcpcd usb0
	fi
	ssh -6 pi@"$(avahi-resolve-host-name raspberrypi.local | awk '{ print $2 }')"%usb0
	sudo systemctl stop avahi-daemon.service
}

screencast() {
	IS_RUNNING="$(pgrep ffmpeg)"
	if [ "$IS_RUNNING" != "" ]; then
		notify-send "Stopped recording.."
		# Kill with SIGTERM, allowing finishing touches
		pkill -15 ffmpeg
		sleep 3
		# Kill with SIGKILL, just in case it is still running
		pkill -9 ffmpeg
	else
		NAME=${1:-"output"}
		[ -n "$2" ] && AUDIO_CODEC="-c:a aac"
		[ -n "$2" ] && AUDIO_SOURCE="-f pulse -ac 2 -i $2" # 1 = system, 2 = mic

		notify-send "Started recording.."
		ffmpeg -y \
			-threads 4 \
			-f x11grab \
			-framerate 24 \
			-s 3000x2000 \
			-i "$DISPLAY.0" \
			$AUDIO_SOURCE \
			-r 24 \
			-vf scale=1500:1000 \
			$AUDIO_CODEC \
			-c:v libx264 -crf 0 -preset ultrafast \
			"$NAME.mkv" &
	fi
}

stream() {
	QUALITY=${2:-"720p"}

	streamlink --player mpv "https://twitch.tv/$1" "$QUALITY" > /dev/null 2>&1 &
}

webmconvert() {
	[ "$2" = "1" ] && AUDIO="-c:a libvorbis" || AUDIO="-an"

	# https://trac.ffmpeg.org/wiki/Encode/H.264
	# https://trac.ffmpeg.org/wiki/Encode/VP8
	# -qmin 0   (0-63, default 4, lower = better quality)
	# -qmax 40  (qmin-63, default 63)
	# -crf 16   (0-51, default 23, 0 = lossless)
	# -b:v 4M
	ffmpeg -threads 4 -i "$1" -c:v libvpx -qmin 0 -qmax 40 -crf 16 -b:v 4M "$AUDIO" "${1%.*}_convert.webm"
}

"$@"
