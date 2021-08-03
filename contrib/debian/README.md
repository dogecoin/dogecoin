
Debian
====================
This directory contains files used to package coingreend/coingreen-qt
for Debian-based Linux systems. If you compile coingreend/coingreen-qt yourself, there are some useful files here.

## coingreen: URI support ##


coingreen-qt.desktop  (Gnome / Open Desktop)
To install:

	sudo desktop-file-install coingreen-qt.desktop
	sudo update-desktop-database

If you build yourself, you will either need to modify the paths in
the .desktop file or copy or symlink your coingreen-qt binary to `/usr/bin`
and the `../../share/pixmaps/bitcoin128.png` to `/usr/share/pixmaps`

coingreen-qt.protocol (KDE)

