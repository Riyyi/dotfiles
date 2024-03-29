import os
import subprocess

# Load autoconfig.yml
config.load_autoconfig()

# Keybinds
# ---------------------------------------- #

# General
config.unbind('d')
config.bind('dd', 'tab-close')
config.bind('go', 'set-cmd-text :open -s {url:pretty}')
config.bind('gO', 'set-cmd-text :open -s -t -r {url:pretty}')
config.bind('o', 'set-cmd-text -s :open -s')
config.bind('O', 'set-cmd-text -s :open -s -t')
config.bind('Sb', 'open -t qute://bookmarks#bookmarks')
config.bind('Sh', 'open -t qute://history')
config.bind('Sq', 'open -t qute://bookmarks')
config.bind('Ss', 'open -t qute://settings')
config.bind('wo', 'set-cmd-text -s :open -s -w')
config.bind('wO', 'set-cmd-text -s :open -s -w {url:pretty}')
config.bind('wp', 'open -s -w -- {clipboard}')
config.bind('wP', 'open -s -w -- {primary}')
config.bind('<<', 'tab-move -')
config.bind('>>', 'tab-move +')
config.bind('<Alt-`>', 'tab-focus last')
config.bind('<F1>', 'config-cycle tabs.show always never')

# Command menu selection
config.bind('<Alt-h>', 'mode-leave', mode='command')
config.bind('<Alt-j>', 'completion-item-focus --history next', mode='command')
config.bind('<Alt-k>', 'completion-item-focus --history prev', mode='command')
config.bind('<Alt-l>', 'command-accept', mode='command')
config.bind('<Ctrl-g>', 'mode-leave', mode='command')

# Legacy bindings
config.bind('<Alt-Left>', 'back')
config.bind('<Alt-Right>', 'forward')
config.bind('<Ctrl-->', 'zoom-out')
config.bind('<Ctrl-=>', 'zoom-in')
config.bind('<Ctrl-0>', 'zoom')
config.bind('<Ctrl-B>', 'open -t qute://bookmarks#bookmarks')
config.bind('<Ctrl-D>', 'bookmark-add')
config.bind('<Ctrl-F>', 'set-cmd-text /')
config.bind('<Ctrl-H>', 'open -t qute://history')
config.bind('<Ctrl-L>', 'set-cmd-text :open -s {url:pretty}')
config.bind('<Ctrl-R>', 'reload')
config.bind('<Ctrl-Shift-R>', 'reload -f')
config.bind('<Ctrl-Shift-T>', 'undo')
config.bind('<Ctrl-Shift-Tab>', 'tab-prev')
config.bind('<Ctrl-T>', ':open -t about:blank ;; set-cmd-text -s :open -s ')
config.bind('<Ctrl-Tab>', 'tab-next')
config.bind('<F6>', 'set-cmd-text :open -s {url:pretty}')
config.bind('<F12>', 'devtools bottom')
config.bind('<Shift-Space>', 'scroll-page 0 -1')
config.bind('<Space>', 'scroll-page 0 0.5')
# j open downloads

# mpv spawning
config.bind('<Ctrl-m>', 'spawn mpv {url} ;; message-info "Sending video to mpv..."')
config.bind('<Ctrl-Shift-m>', 'hint links spawn umpv {hint-url}')
config.bind(';M', 'hint --rapid links spawn umpv {hint-url}')

# Reload browser config
config.bind('<Alt-Shift-r>', 'config-source ;; message-info "qutebrowser reloaded."')

# Run :debug-keytester to get keybinding names

# General
# ---------------------------------------- #

c.auto_save.session = True
c.completion.height = '35%'
#  c.completion.shrink = False
c.content.geolocation = False
c.content.headers.accept_language = 'en-US,en;q=0.5'
c.content.headers.custom = { 'accept': 'text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' }
c.content.headers.do_not_track = True
c.content.headers.referer = 'same-domain'
c.content.headers.user_agent = 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/{upstream_browser_version} Safari/537.36'
c.content.pdfjs = True
c.content.webrtc_ip_handling_policy = 'default-public-interface-only'
c.fonts.default_family = ['DejaVu Sans', 'Source Han Sans JP']
# c.hints.chars = 'asdfghjkl'
c.scrolling.bar = 'always'
c.session.lazy_restore = True
c.spellcheck.languages = ['en-US', 'nl-NL']
c.statusbar.padding = {'top': 1, 'bottom': 3, 'left': 5, 'right': 5}
c.statusbar.position = 'top'
c.tabs.background = True
c.tabs.indicator.width = 0
c.tabs.last_close = 'blank'
c.tabs.padding = {'top': 3, 'bottom': 3, 'left': 5, 'right': 5}
c.tabs.position = 'left'
c.tabs.title.format = '{index}: {audio}{current_title}'
c.tabs.width = 240
c.url.default_page = 'about:blank'
c.url.searchengines = {
    'DEFAULT': 'https://google.com/search?q={}',
    '4': 'https://4chan.org/{}',
    'a': 'https://archlinux.org/packages/?q={}',
    'au': 'https://aur.archlinux.org/packages?K={}',
    'aw': 'https://wiki.archlinux.org/?search={}',
    'i': 'https://google.com/search?tbm=isch&q={}',
    'yt': 'https://youtube.com/results?search_query={}',
}
c.url.start_pages = 'https://google.com'

# Colors
# ---------------------------------------- #

e = os.environ
bgcolor             = e['BGCOLOR']
bgcolor_inactive    = e['BGCOLOR_INACTIVE']
bgcolor_urgent      = e['BGCOLOR_URGENT']
fgcolor             = e['FGCOLOR']
fgcolor_inactive    = e['FGCOLOR_INACTIVE']

darkblack   = e['COLOR0']
black       = e['COLOR8']
darkred     = e['COLOR1']
red         = e['COLOR9']
darkgreen   = e['COLOR2']
green       = e['COLOR10']
darkyellow  = e['COLOR3']
yellow      = e['COLOR11']
darkblue    = e['COLOR4']
blue        = e['COLOR12']
darkmagenta = e['COLOR5']
magenta     = e['COLOR13']
darkcyan    = e['COLOR6']
cyan        = e['COLOR14']
darkwhite   = e['COLOR7']
white       = e['COLOR15']

# Completion colors
c.colors.completion.category.bg = bgcolor
c.colors.completion.category.border.bottom = bgcolor_inactive
c.colors.completion.category.border.top = bgcolor_inactive
c.colors.completion.category.fg = white
c.colors.completion.even.bg = bgcolor
c.colors.completion.fg = white
c.colors.completion.item.selected.bg = blue
c.colors.completion.item.selected.border.bottom = blue
c.colors.completion.item.selected.border.top = blue
c.colors.completion.item.selected.fg = black
c.colors.completion.match.fg = magenta
c.colors.completion.odd.bg = bgcolor_inactive
c.colors.completion.scrollbar.bg = darkwhite
c.colors.completion.scrollbar.fg = white

# Download colors
c.colors.downloads.bar.bg = black
c.colors.downloads.error.bg = red
c.colors.downloads.error.fg = fgcolor
c.colors.downloads.start.bg = blue
c.colors.downloads.start.fg = black
c.colors.downloads.stop.bg = green
c.colors.downloads.stop.fg = black
# c.colors.downloads.system.bg = 'rgb'
# c.colors.downloads.system.fg = 'rgb'

# Hints colors
c.colors.hints.bg = yellow
c.colors.hints.fg = black
c.colors.hints.match.fg = fgcolor

# Keyhint colors
c.colors.keyhint.bg = bgcolor_inactive
c.colors.keyhint.fg = darkwhite
c.colors.keyhint.suffix.fg = white

# Error colors
c.colors.messages.error.bg = darkred
c.colors.messages.error.border = darkred
c.colors.messages.error.fg = fgcolor

# Info colors
c.colors.messages.info.bg = blue
c.colors.messages.info.border = blue
c.colors.messages.info.fg = black

# Warning colors
c.colors.messages.warning.bg = darkyellow
c.colors.messages.warning.border = darkyellow
c.colors.messages.warning.fg = fgcolor

# Prompt colors
c.colors.prompts.bg = bgcolor
c.colors.prompts.border = bgcolor
c.colors.prompts.fg = white
c.colors.prompts.selected.bg = darkwhite

# Satusbar colors
c.colors.statusbar.caret.bg = magenta
c.colors.statusbar.caret.fg = white
c.colors.statusbar.caret.selection.bg = magenta
c.colors.statusbar.caret.selection.fg = white
c.colors.statusbar.command.bg = blue
c.colors.statusbar.command.fg = black
c.colors.statusbar.command.private.bg = darkwhite
c.colors.statusbar.command.private.fg = white
c.colors.statusbar.insert.bg = green
c.colors.statusbar.insert.fg = black
c.colors.statusbar.normal.bg = bgcolor_inactive
c.colors.statusbar.normal.fg = white
c.colors.statusbar.passthrough.bg = darkblue
c.colors.statusbar.passthrough.fg = white
c.colors.statusbar.private.bg = darkwhite
c.colors.statusbar.private.fg = white
c.colors.statusbar.progress.bg = white
c.colors.statusbar.url.error.fg = darkyellow
c.colors.statusbar.url.fg = white
c.colors.statusbar.url.hover.fg = cyan
c.colors.statusbar.url.success.http.fg = darkwhite
c.colors.statusbar.url.success.https.fg = white
c.colors.statusbar.url.warn.fg = yellow

# Tab colors
c.colors.tabs.bar.bg = bgcolor_inactive
c.colors.tabs.even.bg = bgcolor
c.colors.tabs.even.fg = white
c.colors.tabs.indicator.error = red
c.colors.tabs.indicator.start = blue
c.colors.tabs.indicator.stop = bgcolor
# c.colors.tabs.indicator.system = 'rgb'
c.colors.tabs.odd.bg = bgcolor
c.colors.tabs.odd.fg = white
c.colors.tabs.selected.even.bg = bgcolor_inactive
c.colors.tabs.selected.even.fg = white
c.colors.tabs.selected.odd.bg = bgcolor_inactive
c.colors.tabs.selected.odd.fg = white

# Default background color
c.colors.webpage.bg = fgcolor

# Update external dependencies
# ---------------------------------------- #

subprocess.run(['setsid', '-f', 'python', os.path.join(e['XDG_CONFIG_HOME'], 'qutebrowser/update.py')] + c.spellcheck.languages)
