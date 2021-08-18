#!/bin/sh

# Manages dotfiles and packages
# Depends: pacman-contrib

# User-config---------------------------

# File which holds all installed packages
PACKAGE_FILE="packages"

# Files that are stored in the repository but shouldn't get copied (regex)
EXCLUDE_FILES="${0#??}|$PACKAGE_FILE|.*.md$|.*README.org$|.git|screenshot.png"

# Directories that are treated like a system directory (/) (regex)
SYSTEM_DIR='boot|etc|usr/share'

# Arch User Repository helper program name (needs pacman flag compatibility!)
AUR_HELPER="trizen"

# --------------------------------------

if [ "$(dirname "$0")" != "." ]; then
	echo "Please run this script from the directory it resides."
	exit 1
fi

help() {
	B=$(tput bold)
	U=$(tput smul)
	N=$(tput sgr0)

	cat << EOF
${B}NAME${N}
	dotfiles - manages dotfiles and packages

${B}SYNOPSIS${N}
	${B}./dotfiles.sh${N} ${U}OPTION${N} [${U}ARG${N}]

${B}OPTIONS${N}
	${B}-a${N} ${U}FILE${N}, ${B}--add${N} ${U}FILE${N}
		Add file to the dotfiles directory.

	${B}-f, --files${N}
		Display all files added to the dotfiles directory.

	${B}-h, --help${N}
		Display usage message and exit.

	${B}-p${N} [${U}FUNCTION${N}], ${B}--packages${N} [${U}FUNCTION${N}]
		Apply ${U}FUNCTION${N} to the package manager packages.

		${U}install${N}      Install all core packages of the stored list.

		${U}install-aur${N}  Install all AUR packages of the stored list.

		${U}list${N}         Display all packages installed on the system.

		${U}store${N}        Stores a list of all installed packages on the system.

		The default value is ${U}list${N}.

	${B}-l, --pull${N}
		Pull each added file from the system to the dotfiles directory.

	${B}-s, --push${N}
		Push each added file to its location on the system.
EOF
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

set_files() {
	FILES="$(find . -type f -o -type l \
		| awk -v e="^./($EXCLUDE_FILES)" '$0 !~ e { print $0 }')"
}

list_files() {
	# If unset
	[ -z "${FILES+x}" ] && set_files

	# Remove leading ./ from filepaths
	echo "$FILES" | sed 's/^\.\///'
}

add() {
	[ -z "$1" ] && return 1

	FILE="$(readlink -f "$(dirname "$1")")/$(basename "$1")"
	FILE_CUT_HOME="$(echo "$FILE" \
		| awk -v m="^$HOME/" '$0 ~ m { print substr($0, length(m)) }')"

	# /home/<user>/
	if [ -n "$FILE_CUT_HOME" ]; then
		mkdir -p "$(pwd)/$(dirname "$FILE_CUT_HOME")"
		cp -a "$FILE" "$(pwd)/$FILE_CUT_HOME"
	# /
	else
		mkdir -p "$(pwd)/$(dirname "$FILE")"
		sudo cp -a "$FILE" "$(pwd)/$FILE"
	fi
}

pull_push() {
	# If unset or empty string
	[ -z "$1" ] && return 1

	# If unset
	[ -z "${FILES+x}" ] && set_files

	MATCH="^./($SYSTEM_DIR)/"

	# Filter system directories and remove leading ./ from filepaths
	HOME_FILES="$(echo "$FILES" \
			| awk -v m="$MATCH" '$0 !~ m { print substr($0, 3) }')"

	for f in $HOME_FILES; do
		if [ "$1" = "pull" ]; then
			# cp /home/<user>/<file> /[<some dir>/]dotfiles/<file>
			cp -a "$HOME/$f" "$(pwd)/$f"
		elif [ "$1" = "push" ]; then
			mkdir -p "$(dirname "$HOME/$f")"
			cp -a "$(pwd)/$f" "$HOME/$f"
		fi
	done

	# Filter non-system directories and remove leading ./ from filepaths
	SYSTEM_FILES="$(echo "$FILES" \
		| awk -v m="$MATCH" '$0 ~ m { print substr($0, 3) }')"

	for f in $SYSTEM_FILES; do
		if [ "$1" = "pull" ]; then
			# cp /<file> /[<some dir>/]dotfiles/<file>
			sudo cp -a "/$f" "$(pwd)/$f"
		elif [ "$1" = "push" ]; then
			sudo cp -a "$(pwd)/$f" "/$f"
		fi
	done
}

pull() {
	pull_push "pull"
}

push() {
	pull_push "push"
}

packages() {
	if ! pacman -Qqs pacman-contrib > /dev/null; then \
		echo 'Please install the "pacman-contrib" dependency before running this option.'
		exit 1
	fi

	FILTER_LIST="$( (pacman -Qqg base base-devel; pactree -u base | tail -n +2) | sort)"
	PACKAGE_LIST="$(pacman -Qqe | grep -vx "$FILTER_LIST" | sort)"

	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		echo "$PACKAGE_LIST"
	elif [ "$1" = "store" ]; then
		if [ ! -s $PACKAGE_FILE ]; then
			touch "$PACKAGE_FILE"
		else
			truncate -s 0 "$PACKAGE_FILE"
		fi
		echo "$PACKAGE_LIST" > "$PACKAGE_FILE"
	elif [ "$1" = "install" ] || [ "$1" = "install-aur" ]; then
		# Grab everything off enabled official repositories that is in the list
		CORE_LIST="$(pacman -Ssq | grep -xf $PACKAGE_FILE)"

		if [ "$1" = "install" ]; then
			# Install core packages, answer no to pacman questions (honor Ignore)
			yes n | sudo pacman -Sy --needed $CORE_LIST
		fi
		if [ "$1" = "install-aur" ]; then
			# Determine which packages in the list are from the AUR
			AUR_LIST="$(grep -vx "$CORE_LIST" < $PACKAGE_FILE)"

			# Install AUR packages
			"$AUR_HELPER" -Sy --needed --noconfirm $AUR_LIST
		fi
	fi
}

# --------------------------------------

SCRIPT="$(basename "$0")"

# $1 = -option, $2 message
option_wrong() {
	[ -z "$1" ] || [ -z "$2" ] && return 1

	echo "$SCRIPT: $2 '$1'" >&2
	echo "Try '$SCRIPT -h' or '$SCRIPT --help' for more information." >&2
	exit 1
}

# Get all the -options
OPTIONS=$(echo "$@" | awk '{
	for(i=1; i<=NF; i++) { if ($i ~ /^-/) printf "%s ", $i }
}' | wc -w)

# Check if -options are valid
if [ "$OPTIONS" -gt 1 ]; then
	LAST="$(echo "$@" | cut -d ' ' -f $#)"
	option_wrong "$LAST" "option too many"
elif [ "$#" -gt 2 ]; then
	LAST="$(echo "$@" | cut -d ' ' -f $#)"
	option_wrong "$LAST" "argument too many"
fi

OPT="$(echo "$* " | cut -d ' ' -f 1)"
ARG="$(echo "$* " | cut -d ' ' -f 2)"

# Parse -options and call functions
case $OPT in
	-a | --add)
		add "$ARG" || option_wrong "$OPT" 'option requires an argument'
		;;
	-f | --files)
		list_files
		;;
	-h | --help)
		help
		;;
	-p | --packages)
		packages "$ARG"
		;;
	-l | --pull)
		pull
		;;
	-s | --push)
		push
		;;
	*)
		option_wrong "$OPT" 'invalid option'
		;;
esac

# @Todo:
# push function to push just one file
