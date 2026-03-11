Sample init scripts and service configuration for scrapcoind
==========================================================

Sample scripts and configuration files for systemd, Upstart and OpenRC
can be found in the contrib/init folder.

    contrib/init/scrapcoind.service:    systemd service unit configuration
    contrib/init/scrapcoind.openrc:     OpenRC compatible SysV style init script
    contrib/init/scrapcoind.openrcconf: OpenRC conf.d file
    contrib/init/scrapcoind.conf:       Upstart service configuration file
    contrib/init/scrapcoind.init:       CentOS compatible SysV style init script

1. Service User
---------------------------------

All three Linux startup configurations assume the existence of a "scrapcoin" user
and group.  They must be created before attempting to use these scripts.
The OS X configuration assumes scrapcoind will be set up for the current user.

2. Configuration
---------------------------------

At a bare minimum, scrapcoind requires that the rpcpassword setting be set
when running as a daemon.  If the configuration file does not exist or this
setting is not set, scrapcoind will shutdown promptly after startup.

This password does not have to be remembered or typed as it is mostly used
as a fixed token that scrapcoind and client programs read from the configuration
file, however it is recommended that a strong and secure password be used
as this password is security critical to securing the wallet should the
wallet be enabled.

If scrapcoind is run with the "-server" flag (set by default), and no rpcpassword is set,
it will use a special cookie file for authentication. The cookie is generated with random
content when the daemon starts, and deleted when it exits. Read access to this file
controls who can access it through RPC.

By default the cookie is stored in the data directory, but its location can be overridden
with the option '-rpccookiefile'.

This allows for running scrapcoind without having to do any manual configuration.

`conf`, `pid`, and `wallet` accept relative paths which are interpreted as
relative to the data directory. `wallet` *only* supports relative paths.

For an example configuration file that describes the configuration settings,
see `contrib/debian/examples/scrapcoin.conf`.

3. Paths
---------------------------------

3a) Linux

All three configurations assume several paths that might need to be adjusted.

Binary:              `/usr/bin/scrapcoind`  
Configuration file:  `/etc/scrapcoin/scrapcoin.conf`  
Data directory:      `/var/lib/scrapcoind`  
PID file:            `/var/run/scrapcoind/scrapcoind.pid` (OpenRC and Upstart) or `/var/lib/scrapcoind/scrapcoind.pid` (systemd)  
Lock file:           `/var/lock/subsys/scrapcoind` (CentOS)  

The configuration file, PID directory (if applicable) and data directory
should all be owned by the scrapcoin user and group.  It is advised for security
reasons to make the configuration file and data directory only readable by the
scrapcoin user and group.  Access to scrapcoin-cli and other scrapcoind rpc clients
can then be controlled by group membership.

3b) Mac OS X

Binary:              `/usr/local/bin/scrapcoind`  
Configuration file:  `~/Library/Application Support/ScrapCoin/scrapcoin.conf`  
Data directory:      `~/Library/Application Support/ScrapCoin`
Lock file:           `~/Library/Application Support/ScrapCoin/.lock`

4. Installing Service Configuration
-----------------------------------

4a) systemd

Installing this .service file consists of just copying it to
/usr/lib/systemd/system directory, followed by the command
`systemctl daemon-reload` in order to update running systemd configuration.

To test, run `systemctl start scrapcoind` and to enable for system startup run
`systemctl enable scrapcoind`

4b) OpenRC

Rename scrapcoind.openrc to scrapcoind and drop it in /etc/init.d.  Double
check ownership and permissions and make it executable.  Test it with
`/etc/init.d/scrapcoind start` and configure it to run on startup with
`rc-update add scrapcoind`

4c) Upstart (for Debian/Ubuntu based distributions)

Drop scrapcoind.conf in /etc/init.  Test by running `service scrapcoind start`
it will automatically start on reboot.

NOTE: This script is incompatible with CentOS 5 and Amazon Linux 2014 as they
use old versions of Upstart and do not supply the start-stop-daemon utility.

4d) CentOS

Copy scrapcoind.init to /etc/init.d/scrapcoind. Test by running `service scrapcoind start`.

Using this script, you can adjust the path and flags to the scrapcoind program by
setting the SCRAPCOIND and FLAGS environment variables in the file
/etc/sysconfig/scrapcoind. You can also use the DAEMONOPTS environment variable here.

4e) Mac OS X

Copy org.scrapcoin.scrapcoind.plist into ~/Library/LaunchAgents. Load the launch agent by
running `launchctl load ~/Library/LaunchAgents/org.scrapcoin.scrapcoind.plist`.

This Launch Agent will cause scrapcoind to start whenever the user logs in.

NOTE: This approach is intended for those wanting to run scrapcoind as the current user.
You will need to modify org.scrapcoin.scrapcoind.plist if you intend to use it as a
Launch Daemon with a dedicated scrapcoin user.

5. Auto-respawn
-----------------------------------

Auto respawning is currently only configured for Upstart and systemd.
Reasonable defaults have been chosen but YMMV.
