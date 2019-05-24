#!/bin/sh

# Suppress output
NULL=""

# Scroll up, scroll down
if [ "$WM" = "bspwm" ];then
	SWITCH="bspc desktop -f"

	U="A4:$SWITCH prev.local:"
	D="A5:$SWITCH next.local:"
elif [ "$WM" = "i3" ];then
	SWITCH="i3-msg workspace"
	NULL="> /dev/null 2>&1"

	U="A4:$SWITCH prev_on_output $NULL:"
	D="A5:$SWITCH next_on_output $NULL:"
fi

CURRENT=$(xprop -root _NET_CURRENT_DESKTOP | awk '{print $3 + 1}')
WORKSPACES="$(xprop -root '\n$0\n$1\n$2\n$3\n$4\n$5\n$6\n$7\n$8\n$9' _NET_DESKTOP_NAMES \
	| awk -v c="$CURRENT" -v s="$SWITCH" -v n="$NULL" \
	      -v c7="$COLOR7" -v c15="$COLOR15" -v b="$BGCOLOR" -v bi="$BGCOLOR_INACTIVE" '
	/".*"/ {
		name = substr($0, 2, length($0) - 2);

		if (c == FNR - 1)
			printf "%{B%s}  %s  %{B%s}",
			b, name, bi
		else
			printf "%{F%s}%{A:%s \"%s\" %s:}  %s  %{A}%{F%s}",
			c7, s, name, n, name, c15
	}
')"

printf "%s\n" "workspaces%{$U}%{$D}$WORKSPACES%{A}%{A}"
