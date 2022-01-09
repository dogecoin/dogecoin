
Debian
====================
This directory contains files used to package dogecoind/dogecoin-qt
for Debian-based Linux systems. If you compile dogecoind/dogecoin-qt yourself, there are some useful files here.

## dogecoin: URI support ##


dogecoin-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install dogecoin-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your dogecoin-qt binary to `/usr/bin`
and the `../../share/pixmaps/dogecoin128.png` to `/usr/share/pixmaps`

dogecoin-qt.protocol (KDE)

