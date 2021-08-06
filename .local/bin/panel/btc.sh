#!/bin/sh

# Enable mathematics in POSIX shell
calc() { awk "BEGIN { printf(\"%.2f\", $*) }"; }

data() {
	[ -z "$1" ] && return 1

	url="$1"
	curl --location --request GET --silent "$url"
}

output() {
	[ -z "$1" ] && return 1

	# Get symbol and color
	difference="$1"
	possitive=$(calc "$difference >= 0" | cut -c 1)
	if [ "$possitive" -eq 1 ]; then
		symbol=""
		color="$COLOR2"
	else
		symbol=""
		color="$COLOR1"
	fi

	# Result
	echo "%{F$color}$symbol $difference%%{F$COLOR15}"
}

# Get dates
date_last_week_start="$(date --date 'last week' +%s000)"
date_last_week_end=$(calc "$date_last_week_start + (1000 * 60 * 60)") # +1h
date_last_week_end=${date_last_week_end%.00} # cut off ".00" at the end

# API URLs
url="https://api.coincap.io/v2/assets/bitcoin"
url_last_week="${url}/history?interval=h1&start=${date_last_week_start}&end=${date_last_week_end}"

# Current price
data="$(data "$url")"
data_first_character="$(echo "$data" | cut -c 1)"
if [ "$data_first_character" != "{" ]; then echo "rate limit"; exit; fi
price="$(echo "$data" | jq --compact-output --raw-output '.data.priceUsd')"
price=$(calc "$price")

# Get yesterdays difference
difference_yesterday="$(echo "$data" | jq --compact-output --raw-output '.data.changePercent24Hr')"
difference_yesterday="$(calc "$difference_yesterday")"

# Get last weeks difference
data_last_week="$(data "$url_last_week")"
data_last_week_first_character="$(echo "$data_last_week" | cut -c 1)"
if [ "$data_last_week_first_character" != "{" ]; then echo "rate limit"; exit; fi
price_last_week="$(echo "$data_last_week" | jq --compact-output --raw-output '.data[0].priceUsd')"
difference_last_week=$(calc "$price / $price_last_week * 100 - 100")

# Create output formatting
difference_yesterday_output="d $(output "$difference_yesterday")"
difference_last_week_output="w $(output "$difference_last_week")"

echo "\$$price  $difference_yesterday_output  $difference_last_week_output"
