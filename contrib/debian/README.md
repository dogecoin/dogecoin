
Debian
====================
This directory contains files used to package garudacoind/garudacoin-qt
for Debian-based Linux systems. If you compile garudacoind/garudacoin-qt yourself, there are some useful files here.

## garudacoin: URI support ##


garudacoin-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install garudacoin-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your garudacoin-qt binary to `/usr/bin`
and the `../../share/pixmaps/bitcoin128.png` to `/usr/share/pixmaps`

garudacoin-qt.protocol (KDE)

