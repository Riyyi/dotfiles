#!/usr/bin/python

import os
import requests
import sys

arguments = sys.argv
e = os.environ

# Remove script call from argument list
arguments.pop(0)

# Update spellcheck dictionaries
# ---------------------------------------- #

for language in arguments:
	os.system('/usr/share/qutebrowser/scripts/dictcli.py install ' + language)

# Update user scripts
# ---------------------------------------- #

dir = os.path.join(e['XDG_DATA_HOME'], 'qutebrowser/greasemonkey')

scripts = ['https://www.4chan-x.net/builds/4chan-X.user.js',
			'https://greasyfork.org/scripts/395257-better-google/code/Better Google.user.js',
			'https://greasyfork.org/scripts/29420-google-dwimages/code/Google DWIMages.user.js']
for url in scripts:
	r = requests.get(url, allow_redirects = True)
	open(os.path.join(dir, os.path.basename(url)), 'wb').write(r.content)

# Reload greasemonkey
os.system('qutebrowser -C /dev/null :greasemonkey-reload')
