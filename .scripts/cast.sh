#!/bin/sh

FILE_NAME="$1"

TRACKS="$(mkvmerge -J "$FILE_NAME" | jq -c '
	.tracks[] |
	select(.type == "subtitles") |
	{
		id:.id,
		codec_id:.properties.codec_id,
		language:.properties.language,
		track_name:.properties.track_name
	}')"

if [ -n "$2" ]; then
	SELECTION=$2
else
	echo "$TRACKS"
	echo -n "Choose a subtitle ID: "
	read -r SELECTION
fi

if [ -z "$(echo $SELECTION | sed -nr 's/^([0-9]+)$/\1/p')" ]; then
	echo "Please select a number"
	exit
fi

for TRACK in $TRACKS; do
	TRACK_ID=$(echo $TRACK | sed -nr 's/.*"id":([0-9]+),.*/\1/p')
	if [ $TRACK_ID != $SELECTION ]; then
		continue
	fi

	CODEC_ID="$(echo $TRACK | \
		sed -nr 's/.*"codec_id":"(S_[A-Z]+\/?[A-Z]*[0-9]*)",.*/\1/p')"

	if [ "$CODEC_ID" = "S_TEXT/UTF8" ]; then
		EXT="srt"
	elif [ "$CODEC_ID" = "S_TEXT/ASS" ]; then
		EXT="ssa"
	elif [ "$CODEC_ID" = "S_TEXT/USF" ]; then
		EXT="usf"
	elif [ "$CODEC_ID" = "S_VOBSUB" ]; then
		EXT="sub"
	elif [ "$CODEC_ID" = "S_HDMV/PGS" ]; then
		EXT="sup"
	fi

	echo "Extracting subtitle.."
	mkvextract tracks "$FILE_NAME" $TRACK_ID:/tmp/sub."$EXT" 2>&1 >/dev/null

	echo "Converting subtitle.."
	ffmpeg -y -i /tmp/sub."$EXT" /tmp/sub.vtt 2>/dev/null

	echo "Playing file.."
	castnow "$FILE_NAME" --subtitles /tmp/sub.vtt --subtitle-scale 1.2 --subtitle-color FFFFFFFF
done
