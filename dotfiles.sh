#!/bin/sh

# Manages dotfiles and packages
# Depends: GNU getopt, (pacman, pacman-contrib) / (dpkg, apt)

# User-config
# --------------------------------------

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

help()
{
	b=$(tput bold)
	u=$(tput smul)
	n=$(tput sgr0)

	cat << EOF
${b}NAME${n}
	dotfiles - manages dotfiles and packages

${b}SYNOPSIS${n}
	${b}./dotfiles.sh${n} ${u}OPTION${n} [${u}ARG${n}]

${b}OPTIONS${n}
	${b}-a${n} ${u}FILE${n}, ${b}--add${n}=${u}FILE${n}
		Add file to the dotfiles directory.

	${b}-f, --files${n}
		Display all files added to the dotfiles directory.

	${b}-h, --help${n}
		Display usage message and exit.

	${b}-p${n} [${u}FUNCTION${n}], ${b}--packages${n}=[${u}FUNCTION${n}]
		Apply ${u}FUNCTION${n} to the package manager packages.

		${u}install${n}      Install all core packages of the stored list.

		${u}install-aur${n}  Install all AUR packages of the stored list.

		${u}list${n}         Display all packages installed on the system.

		${u}store${n}        Stores a list of all installed packages on the system.

		The default value is ${u}list${n}.

	${b}-l, --pull${n}
		Pull each added file from the system to the dotfiles directory.

	${b}-s, --push${n}
		Push each added file to its location on the system.
EOF
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help >&2 && exit 1

# Files
# --------------------------------------

setFiles()
{
	files="$(find . -type f -o -type l \
		| awk -v e="^./($excludeFiles)" '$0 !~ e { print $0 }')"
}

listFiles()
{
	# If unset
	[ -z "${files+x}" ] && setFiles

	# Remove leading ./ from filepaths
	echo "$files" | sed 's/^\.\///'
}

add()
{
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

pullPush()
{
	# If unset or empty string
	[ -z "$1" ] && return 1

	# If unset
	[ -z "${files+x}" ] && setFiles

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

pull()
{
	pullPush "pull"
}

push()
{
	pullPush "push"
}

# Packages
# --------------------------------------

osDetect()
{
	id="$(sed -nE 's/^ID=(.*)/\1/p' /etc/os-release)"
	idLike="$(sed -nE 's/^ID_LIKE=(.*)/\1/p' /etc/os-release)"

	if [ "$id" = "arch" ]; then
		os="arch"
	elif echo "$idLike" | grep -q 'arch'; then
		os="arch"
	elif [ "$id" = "debian" ] || [ "$id" = "ubuntu" ]; then
		os="debian"
	elif echo "$idLike" | grep -q 'debian'; then
		os="debian"
	elif echo "$idLike" | grep -q 'ubuntu'; then
		os="debian"
	else
		echo "Unsupported operating system." >&2
		exit 1
	fi
}

osDependencies()
{
	if [ "$os" = "arch" ]; then
		binaryDependencyPair="
			pacman:pacman
			pactree:pacman-contrib
		"
	elif [ "$os" = "debian" ]; then
		binaryDependencyPair="
			apt-cache:apt
			apt-mark:apt
			dpkg-query:dpkg
		"
	fi

	for pair in $binaryDependencyPair; do
		binary="$(echo "$pair" | cut -d ':' -f 1)"
		if ! command -v "$binary" > /dev/null; then
			dependency="$(echo "$pair" | cut -d ':' -f 2)"
			echo "Please install the '$dependency' dependency before running this option." >&2
			exit 1
		fi
	done
}

packageList()
{
	if [ "$os" = "arch" ]; then
		filterList="$( (pacman -Qqg base base-devel; pactree -u base | tail -n +2) | sort)"
		packageList="$(pacman -Qqe | grep -vx "$filterList" | sort)"
	elif [ "$os" = "debian" ]; then
		installedList="$(dpkg-query --show --showformat='${Package}\t${Priority}\n')"
		filterList="$(echo "$installedList" | grep -E 'required|important|standard' | cut -f 1)"
		installedList="$(echo "$installedList" | cut -f 1)"
		installedManuallyList="$(awk '/Commandline:.* install / && !/APT::/ { print $NF }' /var/log/apt/history.log)"
		installedManuallyList="$( (echo "$installedManuallyList"; apt-mark showmanual) | sort -u)"
		packageList="$(echo "$installedManuallyList" | grep -x "$installedList" | grep -vx "$filterList")"
	fi
}

packageInstall()
{
	if [ "$os" = "arch" ]; then
		# Grab everything off enabled official repositories that is in the list
		repoList="$(pacman -Ssq | grep -xf $packageFile)"

		if [ "$1" = "install" ]; then
			# Install packages
			echo "$repoList" | xargs --open-tty sudo pacman -Sy --needed
		fi
		if [ "$1" = "install-aur" ]; then
			# Determine which packages in the list are from the AUR
			aurList="$(grep -vx "$repoList" < $packageFile)"

			# Install AUR packages
			echo "$aurList" | xargs --open-tty "$aurHelper" -Sy --needed --noconfirm
		fi
	elif [ "$os" = "debian" ]; then
		# Grab everything off enabled official repositories that is in the list
		repoList="$(apt-cache search .* | cut -d ' ' -f 1 | grep -xf $packageFile)"

		# Install packages
		echo "$repoList" | xargs --open-tty sudo apt install
	fi
}

packages()
{
	# If unset
	if [ -z "$os" ]; then
		osDetect
		osDependencies
		packageList
	fi

	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		echo "$packageList"
	elif [ "$1" = "store" ]; then
		echo "$packageList" > "$packageFile"
	elif [ "$1" = "install" ]; then
		packageInstall "install"
	elif [ "$1" = "install-aur" ]; then
		packageInstall "install-aur"
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
			listFiles
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
