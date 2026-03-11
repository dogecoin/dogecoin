
Debian
====================
This directory contains files used to package scrapcoind/scrapcoin-qt
for Debian-based Linux systems. If you compile scrapcoind/scrapcoin-qt yourself, there are some useful files here.

## scrapcoin: URI support ##


scrapcoin-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install scrapcoin-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your scrapcoin-qt binary to `/usr/bin`
and the `../../share/pixmaps/scrapcoin128.png` to `/usr/share/pixmaps`

scrapcoin-qt.protocol (KDE)

