#!/bin/sh

# Suppress output
NULL="> /dev/null 2>&1"

# Scroll up, scroll down
U="A4:i3-msg workspace prev_on_output $NULL:"
D="A5:i3-msg workspace next_on_output $NULL:"

# i3
# WMNAME="$(xprop -root '\t$0' _NET_WM_NAME | cut -f 2 | tr -d '"')"

CURRENT=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3 + 1}')
WORKSPACES="$(xprop -root '\n$0\n$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9' _NET_DESKTOP_NAMES \
	| awk -v c="$CURRENT" -v n="$NULL" \
	      -v c7="$COLOR7" -v c15="$COLOR15" -v b="$BGCOLOR" -v bi="$BGCOLOR_INACTIVE" '
	/".*"/ {
		name = substr($0, 2, length($0) - 2);

		if (c == FNR - 1)
			printf "%{B%s}  %s  %{B%s}",
			b, name, bi
		else
			printf "%{F%s}%{A:i3-msg workspace %s %s:}  %s  %{A}%{F%s}",
			c7, name, n, name, c15
	}
')"

printf "%s\n" "workspaces%{$U}%{$D}$WORKSPACES%{A}%{A}"
