
Debian
====================
This directory contains files used to package dingocoind/dingocoin-qt
for Debian-based Linux systems. If you compile dingocoind/dingocoin-qt yourself, there are some useful files here.

## dingocoin: URI support ##


dingocoin-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install dingocoin-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your dingocoin-qt binary to `/usr/bin`
and the `../../share/pixmaps/bitcoin128.png` to `/usr/share/pixmaps`

dingocoin-qt.protocol (KDE)

