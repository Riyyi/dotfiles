#!/bin/sh

# Manages dotfiles and packages
# Depends: GNU getopt, pacman-contrib

# User-config---------------------------

# File which holds all installed packages
packageFile="packages"

# Files that are stored in the repository but shouldn't get copied (regex)
excludeFiles="${0#??}|$packageFile|.*.md$|.*README.org$|.git|screenshot.png"

# Directories that are treated like a system directory (/) (regex)
systemDir='boot|etc|usr/share'

# Arch User Repository helper program name (needs pacman flag compatibility!)
aurHelper="trizen"

# --------------------------------------

if [ "$(dirname "$0")" != "." ]; then
	echo "Please run this script from the directory it resides." >&2
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
	${B}-a${N} ${U}FILE${N}, ${B}--add${N}=${U}FILE${N}
		Add file to the dotfiles directory.

	${B}-f, --files${N}
		Display all files added to the dotfiles directory.

	${B}-h, --help${N}
		Display usage message and exit.

	${B}-p${N} [${U}FUNCTION${N}], ${B}--packages${N}=[${U}FUNCTION${N}]
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
[ "$#" -eq 0 ] && help >&2 && exit 1

set_files() {
	files="$(find . -type f -o -type l \
		| awk -v e="^./($excludeFiles)" '$0 !~ e { print $0 }')"
}

list_files() {
	# If unset
	[ -z "${files+x}" ] && set_files

	# Remove leading ./ from filepaths
	echo "$files" | sed 's/^\.\///'
}

add() {
	[ -z "$1" ] && return 1

	file="$(readlink -f "$(dirname "$1")")/$(basename "$1")"
	fileCutHome="$(echo "$file" \
		| awk -v m="^$HOME/" '$0 ~ m { print substr($0, length(m)) }')"

	# /home/<user>/
	if [ -n "$fileCutHome" ]; then
		mkdir -p "$(pwd)/$(dirname "$fileCutHome")"
		cp -a "$file" "$(pwd)/$fileCutHome"
	# /
	else
		mkdir -p "$(pwd)/$(dirname "$file")"
		sudo cp -a "$file" "$(pwd)/$file"
	fi
}

pull_push() {
	# If unset or empty string
	[ -z "$1" ] && return 1

	# If unset
	[ -z "${files+x}" ] && set_files

	match="^./($systemDir)/"

	# Filter system directories and remove leading ./ from filepaths
	homeFiles="$(echo "$files" \
			| awk -v m="$match" '$0 !~ m { print substr($0, 3) }')"

	for f in $homeFiles; do
		if [ "$1" = "pull" ]; then
			# cp /home/<user>/<file> /[<some dir>/]dotfiles/<file>
			cp -a "$HOME/$f" "$(pwd)/$f"
		elif [ "$1" = "push" ]; then
			mkdir -p "$(dirname "$HOME/$f")"
			cp -a "$(pwd)/$f" "$HOME/$f"
		fi
	done

	# Filter non-system directories and remove leading ./ from filepaths
	systemFiles="$(echo "$files" \
		| awk -v m="$match" '$0 ~ m { print substr($0, 3) }')"

	for f in $systemFiles; do
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
		echo 'Please install the "pacman-contrib" dependency before running this option.' >&2
		exit 1
	fi

	filterList="$( (pacman -Qqg base base-devel; pactree -u base | tail -n +2) | sort)"
	packageList="$(pacman -Qqe | grep -vx "$filterList" | sort)"

	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		echo "$packageList"
	elif [ "$1" = "store" ]; then
		if [ ! -s $packageFile ]; then
			touch "$packageFile"
		else
			truncate -s 0 "$packageFile"
		fi
		echo "$packageList" > "$packageFile"
	elif [ "$1" = "install" ] || [ "$1" = "install-aur" ]; then
		# Grab everything off enabled official repositories that is in the list
		coreList="$(pacman -Ssq | grep -xf $packageFile)"

		if [ "$1" = "install" ]; then
			# Install core packages, answer no to pacman questions (honor Ignore)
			yes n | sudo pacman -Sy --needed $coreList
		fi
		if [ "$1" = "install-aur" ]; then
			# Determine which packages in the list are from the AUR
			aurList="$(grep -vx "$coreList" < $packageFile)"

			# Install AUR packages
			"$aurHelper" -Sy --needed --noconfirm $aurList
		fi
	fi
}

# Option handling
# --------------------------------------

script="$(basename "$0")"
options="$(getopt --options "ha:fp::ls" --longoptions "help,add:,files,packages::,pull,push" -n "$script" -- "$@" 2>&1)"
result="$?"

# Exit if invalid option is provided
if [ "$result" -ne 0 ]; then
	echo "$options" | head -n 1 >&2
	echo "Try './$script --help' for more information." >&2
	exit 1
fi

eval set -- "$options"

while true; do
	case "$1" in
		-a | --add)
			add "$2"
			shift 2
			;;
		-f | --files)
			list_files
			shift
			;;
		-h | --help)
			help
			exit
			;;
		-p | --packages)
			packages "$2"
			shift 2
			;;
		-l | --pull)
			pull
			shift
			;;
		-s | --push)
			push
			shift
			;;
		--)
			shift
			break
			;;
		*)
			break
			;;
	esac
done

# @Todo:
# push function to push just one file
