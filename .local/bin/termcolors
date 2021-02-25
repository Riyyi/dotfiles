#!/bin/sh

# This file echoes a bunch of color codes to the terminal to demonstrate what's
# available. Each line is the color code of one forground color, out of 17
# (default + 16 escapes), followed by a test use of that color on all nine
# background colors (default + 8 escapes).

# The test text
text="gYw"

printf "\n                 40m     41m     42m     43m\
     44m     45m     46m     47m\n";

for fgs in "    m" "   1m" "  30m" "1;30m" "  31m" "1;31m" "  32m" \
           "1;32m" "  33m" "1;33m" "  34m" "1;34m" "  35m" "1;35m" \
           "  36m" "1;36m" "  37m" "1;37m"; do
	fg=$(echo "$fgs" | tr -d ' ')
	printf " %s \033[%s  %s  " "$fgs" "$fg" "$text"
	for bg in 40m 41m 42m 43m 44m 45m 46m 47m; do
		printf " \033[%s\033[%s  %s  \033[0m" "$fg" "$bg" "$text";
	done
	echo
done
echo
