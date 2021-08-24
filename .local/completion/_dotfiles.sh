#compdef dotfiles.sh

typeset -A opt_args

# Argument definition
# ------------------------------------------

_dotfiles_operations=(
	{-F-,--file}'[display all files added to the dotfiles directory]'
	{-P-,--package}'[instal, print or store packages]'
	'(-)'{-h,--help}'[display usage message and exit]'
)

_dotfiles_file_options=(
	'(-F --file)'{-F,--file}
	'(-a --add)'{-a,--add}'[add selected file paths to the dotfiles directory]'
	'(-l --pull)'{-l,--pull}'[pull file(s) from the system to the dotfiles directory]'
	'(-s --push)'{-s,--push}'[push file(s) from the dotfiles directory to the system]'
)

_dotfiles_package_options=(
	'(-P --package)'{-P,--package}
	'(-a --aur-install)'{-a,--aur-install}'[install all AUR packages of the stored list]'
	'(-i --install)'{-i,--install}'[install all official packages of the stored list]'
	'(-s --store)'{-s,--store}'[stores a list of all installed packages on the system]'
)

_dotfiles_dotfile_directory_target=(
	'*:file:_files'
)

_dotfiles_home_directory_target=(
	'*:file:_files -W ~/'
)

# Argument handling
# ------------------------------------------

function _dotfiles_file_argument_handling()
{
	case "$tmp" in
		-*)
			_arguments -s : "$_dotfiles_file_options[@]" && result=0
			;;
		*)
			# Enable matching of dotfiles without explicitly specifying the dot
			setopt globdots

			case "$args" in
				*a*)
					_arguments -s : "$_dotfiles_home_directory_target[@]" && result=0
					;;
				*)
					_arguments -s : "$_dotfiles_dotfile_directory_target[@]" && result=0
					;;
			esac
			;;
	esac
}

function _dotfiles_package_argument_handling()
{
	case "$tmp" in
		-*)
			_arguments -s : "$_dotfiles_package_options[@]" && result=0
			;;
		*)
			case "$args" in
				P)
					_arguments -s : '*:package:_dotfiles_completions_tracked_packages' && result=0
					;;
				*)
					;;
			esac
			;;
	esac
}

if ! zmodload -s zsh/mapfile; then
	function _dotfiles_completions_tracked_packages() {}
else
	function _dotfiles_completions_tracked_packages()
	{
		local -a packages
		packages=( "${(f@)${mapfile[packages]%$'\n'}}" )
		readonly packages
		compadd "$@" -a packages
	}
fi

# Completion control flow
# ------------------------------------------

_dotfiles.sh()
{
	local result=1
	local -a args
	local tmp

	# Get all short flags
	args=( ${${${(M)words:#-*}#-}:#-*} )
	# Get the current flag
	tmp="$words[-1]"

	case "$args" in
		h*)
			_message -e arguments "no more arguments"
			;;
		F*)
			_dotfiles_file_argument_handling
			;;
		P*)
			_dotfiles_package_argument_handling
			;;
		*)
			case ${(M)words:#--*} in
				*--file*)
					_dotfiles_file_argument_handling
					;;
				*--package*)
					_dotfiles_package_argument_handling
					;;
				*)
					_arguments -s : "$_dotfiles_operations[@]" && result=0
					;;
			esac
			;;
	esac

	return result
}

_dotfiles.sh
