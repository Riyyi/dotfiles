#!/bin/sh

# Tracks dotfiles and packages
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
	echo "${b}${red}Error:${n} please run this script from the directory it resides." >&2
	exit 1
fi

help()
{
	(cat << EOF
.TH DOTFILES.SH 1 "2021-08-22" "dotfiles.sh 0.9" "dotfiles.sh Manual"

.SH NAME
dotfiles.sh \- config file and package tracking utility

.SH SYNOPSIS
.B ./dotfiles.sh
.I OPERATION
.RI [ OPTION ...]\&
.RI [ TARGET ...]

.SH DESCRIPTION
dotfiles.sh is a config file and package tracking utility that tracks installed packages on a Linux system.
It features listing and tracking of config files and packages, and the ability to install the tracked packages.

Currently, package tracking is only supported on APT and Pacman based distributions.

Invoking dotfile.sh involves specifying an operation with any potential options and targets to operate on.
A \fItarget\fR is usually a file name, directory or a package name.
Targets can be provided as command line arguments.
Additionally, if a single hyphen (-) is passed as an argument, targets will be read from stdin.

.SH OPERATIONS
.TP
.BR \-F ", " \-\-file
Operate on config files.
This operation allows you to sync config files between the system and the dotfiles directory.
In the first case, if no file names are provided in the command line, all files will be selected.
See File Options below.

.TP
.BR \-P ", " \-\-package
Operate on packages.
This operation allows you to track installed packages and reinstall them.
In the first case, if no package names are provided in the command line, all packages will be selected.
See Package Options below.

.TP
.BR \-h ", " \-\-help
Display usage message and exit.

.SH FILE OPTIONS (APPLY TO -F)
.TP
.BR \-a ", " \-\-add
Add selected file \fIpaths\fR to the dotfiles directory.

.TP
.BR \-l ", " \-\-pull
Pull every (selected) \fIfile\fR from the system to the dotfiles directory.

.TP
.BR \-s ", " \-\-push
Push every (selected) \fIfile\fR from the dotfiles directory to the system.

.SH PACKAGE OPTIONS (APPLY TO -P)
.TP
.BR \-a ", " \-\-aur-install
Install all AUR packages of the stored list.

.TP
.BR \-i ", " \-\-install
Install all official packages of the stored list.

.TP
.BR \-s ", " \-\-store
Stores a list of all installed packages on the system.

.SH EXAMPLES
Usage examples:

$ \fB./dotfiles.sh\fR -Fa ~/.zshrc /etc/zsh/zshenv
.br
\h'4'Add config files to the dotfiles directory

$ \fB./dotfiles.sh\fR -Pia
.br
\h'4'Install all tracked official and AUR packages

.SH AUTHOR
Riyyi
EOF
	) | man -l -
}

# Exit if no option is provided
[ "$#" -eq 0 ] && help && exit 1

# Files
# --------------------------------------

getFileList()
{
	fileList="$(find . -type f -o -type l \
		| awk -v e="^./($excludeFiles)" '$0 !~ e { print $0 }')"
}

separateFileList()
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

filterFileList()
{
	if [ -z "$homeFileList" ] || [ -z "$systemFileList" ]; then
		separateFileList
	fi

	# Filter on provided file name
	homeFileListLoop="$(echo "$homeFileList" | grep "^$1")"
	systemFileListLoop="$(echo "$systemFileList" | grep "^$1")"
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
		sudo mkdir -p "$(pwd)/$(dirname "$file")"
		sudo cp -a "$file" "$(pwd)/$file"
	fi
}

filePull()
{
	filterFileList "$1"

	for file in $homeFileListLoop; do
		# /home/<user>/<file>  ->  dotfiles/<file>
		cp -a "$HOME/$file" "$(pwd)/$file"
	done

	for file in $systemFileListLoop; do
		# /<file>  ->  dotfiles/<file>
		sudo cp -a "/$file" "$(pwd)/$file"
	done
}

filePush()
{
	filterFileList "$1"

	for file in $homeFileListLoop; do
		# dotfiles/<file>  ->  /home/<user>/<file>
		mkdir -p "$(dirname "$HOME/$file")"
		cp -a "$(pwd)/$file" "$HOME/$file"
	done

	for file in $systemFileListLoop; do
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
		echo "${b}${red}Error:${n} unsupported operating system." >&2
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
			echo "${b}${red}Error:${n} required dependency '$dependency' is missing." >&2
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
		-h | --help)
			help
			exit
			;;
		-F | --file)
			if [ -n "$mode" ]; then
				echo "${b}${red}Error:${n} only one operation may be used at a time." >&2
				exit 1
			fi
			mode="file"
			shift
			;;
		-P | --package)
			if [ -n "$mode" ]; then
				echo "${b}${red}Error:${n} only one operation may be used at a time." >&2
				exit 1
			fi
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

# Target parsing
# --------------------------------------

targets="$*"
targetsNoHyphen="$(echo "$targets" | sed -E 's/(^-$|\s-|-\s)//g; s/(\s-\s)/ /;')"

# Read targets from stdin
if [ "$targets" != "$targetsNoHyphen" ]; then
	if [ -t 0 ]; then
		echo "${b}${red}Error:${n} argument '-' specified without input on stdin." >&2
		exit 1
	fi
	eval set -- "$targetsNoHyphen $(cat /dev/stdin)"
fi

# Execute
# --------------------------------------

if [ -z "$mode" ]; then
	echo "${b}${red}Error:${n} no operation specified (use -h for help)" >&2
	exit 1
fi

if [ "$mode" = "file" ]; then
	if [ -z "$options" ]; then
		files "list" "$@"
		exit
	fi

	for option in $options; do
		if [ -z "$*" ]; then
			if [ "$option" = "add" ];then
				echo "${b}${red}Error:${n} no files or directories selected to add." >&2
				exit 1
			fi
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
