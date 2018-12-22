// Display a blank new tab
user_pref("browser.newtabpage.enabled", false);
user_pref("browser.newtab.url", "about:blank");

// Fix dark GTK themes
user_pref("widget.content.gtk-theme-override", "Arc");

// No full screen warning
user_pref("full-screen-api.warning.timeout", 0);

// Disable tracking
user_pref("extensions.pocket.enabled", false);
user_pref("extensions.screenshots.disabled", true);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("dom.ipc.plugins.flash.subprocess.crashreporter.enabled", false);
user_pref("geo.enabled", false);
