#!/bin/sh

# Bulk rename files inside a directory
# Depends:

# User-config-------------------------------

# Define editor to open file in
set -- emacsclient -a ''

# ------------------------------------------

# Output formatting
B=$(tput bold)
L=$(tput setf 1) # Blue
N=$(tput sgr0)

format() {
	echo "${B}${L}::${N}${B} ${1}${N}"
}

cleanup() {
	[ -f "$tmpfile" ] && rm -f "$tmpfile"
	exit "${1:-0}"
}

signal() {
	echo
	cleanup 1
}

# ------------------------------------------

# Get all filenames in the current directory
files=$(find . -maxdepth 1 -type f -printf '%P\n' | sort)

# Write all filesnames to tmpfile
tmpfile=$(mktemp)
trap signal HUP INT TERM
echo "$files" > "$tmpfile"

# Open tmpfile in editor, wait to continue
md5_before=$(md5sum "$tmpfile")
format "Waiting for editor..."
"$@" "$tmpfile" > /dev/null
md5_after=$(md5sum "$tmpfile")

# Exit if nothing was changed
[ "$md5_before" = "$md5_after" ] && cleanup 1

# Exit on mismatch entry size
lines_before=$(echo "$files" | wc -l)
lines_after=$(wc -l "$tmpfile" | cut -d ' ' -f 1)
if [ "$lines_before" != "$lines_after" ]; then
	echo "File amount mismatch!"
	cleanup 1
fi

# Exit if file has a space
if grep -Fq ' ' "$tmpfile"; then
	echo "Filenames with spaces are not supported!"
	cleanup 1
fi

# Create file commands
i=1
for file in $files; do
	newFile=$(sed -n "${i}p" "$tmpfile")

	if [ "$file" = "$newFile" ]; then
		command=""
	else
		[ -z "$newFile" ] && command="rm -f ${file}" || command="mv ${file} \0"
	fi

	sed -i "${i}s/.*/${command}/;" "$tmpfile" > /dev/null

	i=$((i+1))
done

# Delete empty lines
sed -Ei '/^\s*$/d' "$tmpfile"

# Display commands
prompt=$(cat "$tmpfile")
printf "\n%s\n\n" "$prompt"

# Confirm with user
prompt=$(format "Proceed with execution? [Y/n]")
printf "%s " "$prompt"
read -r execute

# Execute file commands
if [ "$execute" = "" ] || [ "$execute" = "y" ] || [ "$execute" = "Y" ]; then
	format "Executing commands..."
	sh "$tmpfile"
else
	cleanup 1
fi

cleanup
