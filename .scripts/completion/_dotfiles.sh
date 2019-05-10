#compdef dotfiles.sh

_dotfiles.sh() {
	integer ret=1
	local -a args

	args+=(
		'(- *)'{-a,--add}'[add file to the dotfiles directory]:file:_files'
		'(- *)'{-f,--files}'[display all files added to the dotfiles directory]'
		'(- *)'{-h,--help}'[display usage message and exit]'
		'(- *)'{-p,--packages}'[instal, list or store packages]:packages:((
			install\:"install all core packages of the stored list"
			list\:"display all packages installed on the system (default)"
			store\:"stores a list of all installed packages"
		))'
		'(- *)'{-l,--pull}'[pull each added file from system to dotfiles directory]'
		'(- *)'{-s,--push}'[push each added file to its location on the system]'
	)

	_arguments $args[@] && ret=0
	return ret
}

_dotfiles.sh
