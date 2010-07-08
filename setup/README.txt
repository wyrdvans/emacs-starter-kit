Setup Emacs to run as a daemon on Mac OS X
- Copy gnu.emacs.daemon.plist to ~/Library/LaunchAgents
  - If not using macports emacs, change the path to your emacs location in the .plist file.
- Execute:  launchctl load -w ~/Library/LaunchAgents/gnu.emacs.daemon.plist
-
