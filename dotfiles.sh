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

b="$(tput bold)"
red="$(tput setf 4)"
n="$(tput sgr0)"

if [ "$(dirname "$0")" != "." ]; then
	echo "${b}${red}Error: Please run this script from the directory it resides.${n}" >&2
	exit 1
fi

help()
{
	u=$(tput smul)

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

getFileList()
{
	fileList="$(find . -type f -o -type l \
		| awk -v e="^./($excludeFiles)" '$0 !~ e { print $0 }')"
}

getFilteredFileLists()
{
	[ -z "$fileList" ] && getFileList

	match="^./($systemDir)/"

	# Filter system directories and remove leading ./ from filepaths
	homeFileList="$(echo "$fileList" \
			| awk -v m="$match" '$0 !~ m { print substr($0, 3) }')"

	# Filter non-system directories and remove leading ./ from filepaths
	systemFileList="$(echo "$fileList" \
		| awk -v m="$match" '$0 ~ m { print substr($0, 3) }')"
}

fileAdd()
{
	[ -z "$1" ] && exit 1

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

filePull()
{
	if [ -z "$homeFileList" ] || [ -z "$systemFileList" ]; then
		getFilteredFileLists
	fi

	for file in $homeFileList; do
		# /home/<user>/<file>  ->  dotfiles/<file>
		cp -a "$HOME/$file" "$(pwd)/$file"
	done

	for file in $systemFileList; do
		# /<file>  ->  dotfiles/<file>
		sudo cp -a "/$file" "$(pwd)/$file"
	done
}

filePush()
{
	if [ -z "$homeFileList" ] || [ -z "$systemFileList" ]; then
		getFilteredFileLists
	fi

	for file in $homeFileList; do
		# dotfiles/<file>  ->  /home/<user>/<file>
		mkdir -p "$(dirname "$HOME/$file")"
		cp -a "$(pwd)/$file" "$HOME/$file"
	done

	for file in $systemFileList; do
		# dotfiles/<file>  ->  /<file>
		sudo mkdir -p "$(dirname "/$file")"
		sudo cp -a "$(pwd)/$file" "/$file"
	done
}

files()
{
	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		[ -z "$fileList" ] && getFileList
		# Remove leading ./ from filepaths
		echo "$fileList" | sed 's/^\.\///' | grep "$2"
	elif [ "$1" = "add" ]; then
		fileAdd "$2"
	elif [ "$1" = "pull" ]; then
		filePull "$2"
	elif [ "$1" = "push" ]; then
		filePush "$2"
	fi
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

getPackageList()
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

		if [ "$1" = "aur-install" ]; then
			# Determine which packages in the list are from the AUR
			aurList="$(grep -vx "$repoList" < $packageFile)"

			# Install AUR packages
			echo "$aurList" | xargs --open-tty "$aurHelper" -Sy --needed --noconfirm
		elif [ "$1" = "install" ]; then
			# Install packages
			echo "$repoList" | xargs --open-tty sudo pacman -Sy --needed
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
	fi

	if [ "$1" = "list" ] || [ "$1" = "" ]; then
		[ -z "$packageList" ] && getPackageList
		echo "$packageList" | grep "$2"
	elif [ "$1" = "store" ]; then
		[ -z "$packageList" ] && getPackageList
		echo "$packageList" > "$packageFile"
	elif [ "$1" = "aur-install" ]; then
		packageInstall "aur-install"
	elif [ "$1" = "install" ]; then
		packageInstall "install"
	fi
}

# Option parsing
# --------------------------------------

script="$(basename "$0")"
parsed="$(getopt --options "hFPails" \
				  --longoptions "help,file,package,add,aur-install,install,pull,push,store" \
				  -n "$script" -- "$@" 2>&1)"
result="$?"

# Exit if invalid option is provided
if [ "$result" -ne 0 ]; then
	echo "$parsed" | head -n 1 >&2
	echo "Try './$script --help' for more information." >&2
	exit 1
fi

eval set -- "$parsed"

while true; do
	case "$1" in
		-F | --file)
			[ -n "$mode" ] && echo "${b}${red}Error: only one operation may be used at a time." >&2 && exit 1
			mode="file"
			shift
			;;
		-P | --package)
			[ -n "$mode" ] && echo "${b}${red}Error: only one operation may be used at a time." >&2 && exit 1
			mode="package"
			shift
			;;
		-a | --add | --aur-install)
			[ "$mode" = "file" ] && options="${options}add "
			[ "$mode" = "package" ] && options="${options}aur-install "
			shift
			;;
		-i | --install)
			options="${options}install "
			shift
			;;
		-l | --pull)
			options="${options}pull "
			shift
			;;
		-s | --push | --store)
			[ "$mode" = "file" ] && options="${options}push "
			[ "$mode" = "package" ] && options="${options}store "
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
# Target parsing
# --------------------------------------

targets="$*"
targetsNoHyphen="$(echo "$targets" | sed -E 's/(^-$|\s-|-\s)//g; s/(\s-\s)/ /;')"

# Read targets from stdin
if [ "$targets" != "$targetsNoHyphen" ]; then
	[ -t 0 ] && echo "${b}${red}Error: argument '-' specified without input on stdin." >&2 && exit 1
	eval set -- "$targetsNoHyphen $(cat /dev/stdin)"
fi

# Execute
# --------------------------------------

if [ "$mode" = "file" ]; then
	if [ -z "$options" ]; then
		files "list" "$@"
		exit
	fi

	for option in $options; do
		if [ -z "$*" ]; then
			[ "$option" = "add" ] && echo "${b}${red}Error: No files or directories selected to add.${n}" >&2 && exit 1
			files "$option"
			continue
		fi

		for path; do
			files "$option" "$path"
		done
	done
fi

if [ "$mode" = "package" ]; then
	if [ -z "$options" ]; then
		packages "list" "$@"
		exit
	fi

	for option in $options; do
		packages "$option"
	done
fi
