
Debian
====================
This directory contains files used to package pepecoind/pepecoin-qt
for Debian-based Linux systems. If you compile pepecoind/pepecoin-qt yourself, there are some useful files here.

## pepecoin: URI support ##


pepecoin-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install pepecoin-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your pepecoin-qt binary to `/usr/bin`
and the `../../share/pixmaps/pepecoin128.png` to `/usr/share/pixmaps`

pepecoin-qt.protocol (KDE)

