#!/bin/sh

# Enable mathematics in POSIX shell
calc() { awk "BEGIN { printf(\"%.2f\", $*) }"; }

output() {
	[ -z "$1" ] || [ -z "$2" ] && return 1

	url="$1"
	price="$2"

	price_old="$(curl -s $url | jq -cr '.data.amount')"
	difference=$(calc "$price / $price_old * 100 - 100")

	# Get symbol and color
	old_higher=$(calc "$price < $price_old" | cut -c 1)
	if [ $old_higher -eq 0 ]; then
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
date_yesterday="$(date -d 'yesterday' +'%Y-%m-%d')"
date_last_week="$(date -d 'last week' +'%Y-%m-%d')"

# Get API URLs
url="https://api.coinbase.com/v2/prices/BTC-USD/spot"
url_yesterday="${url}?date=$date_yesterday"
url_last_week="${url}?date=$date_last_week"

# Current price
price="$(curl -s $url | jq -cr '.data.amount')"

# Create output formatting
difference_yesterday_output="d $(output $url_yesterday $price)"
difference_last_week_output="w $(output $url_last_week $price)"

echo "\$$price  $difference_yesterday_output  $difference_last_week_output"
