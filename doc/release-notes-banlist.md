Misbehaving peers are discouraged, not banned
--------------------------------------------

This change fixes CVE-2020-14198. It backports Bitcoin Core #19219, with the
parts of #14929 and #15617 that #19219 depends on.

Before this change, a peer whose misbehavior score reached `-banscore` was
added to the ban list for `-bantime` (24 hours by default). The ban list had no
size limit. A remote attacker with many addresses could make it grow without
bound, together with `banlist.dat` and the time spent checking every
connection against it.

Changes that operators can see:

- **Misbehaving peers are discouraged, not banned.** The node disconnects the
  peer and records its address in a fixed-size, in-memory filter of about
  50,000 recent addresses. The filter is not saved to disk. It cannot be
  listed, and `setban remove` and `clearbanned` do not change it. A restart
  clears it, and older addresses roll out of it as new ones are added.
  Whitelisted and `-addnode` peers are still never punished. Local peers are
  disconnected, but not discouraged.

- **Misbehavior no longer adds entries to `listbanned` or `banlist.dat`.**
  Bans made with `setban` or the GUI work as before. After an upgrade, the
  list can still hold automatic entries made by an earlier version (see
  *Upgrade* below). Once those expire or are removed, it holds only manual
  bans. The `listbanned` response keeps its fields, including `ban_reason`,
  and `banlist.dat` keeps its format.

- **Discouraged peers can reconnect while inbound slots remain.** An inbound
  connection from a discouraged address is accepted while the node has more
  than one free inbound slot. When the node needs a slot, it first protects
  peers by the existing criteria (network group, ping time, recent
  transactions and blocks, and connection time). If any of the remaining peers
  are discouraged, it chooses the peer to evict from those peers only. When at most one inbound
  slot is free, connections from discouraged addresses are refused. Before
  this change, an automatically banned address was refused for 24 hours,
  whatever the number of free slots. As before, inbound connections from
  manually banned addresses are refused unless the peer is whitelisted.

- **Outbound connections and address relay.** The node does not choose
  discouraged or manually banned addresses for automatic outbound
  connections. A connection to an address that the operator names explicitly
  is still made. The node does not relay or store discouraged addresses
  received in `addr` messages, and does not return them in replies to
  `getaddr`. Manually banned addresses are not filtered there, as before this
  change.

- **Help text.** `-banscore` now describes the threshold for disconnecting and
  discouraging misbehaving peers. `-bantime` now describes the default
  duration of manual bans only. The `listbanned` help and the `setban remove`
  error now say "manually banned".

- **Log text.** `BAN THRESHOLD EXCEEDED` is now
  `DISCOURAGE THRESHOLD EXCEEDED`. A new line, `Disconnecting and
  discouraging peer <address>!`, is logged when such a peer is disconnected. The local-peer warning now reads
  `disconnecting but not discouraging local peer`. With `-debug=net`, a
  refused inbound connection logs `connection from <address> dropped
  (discouraged)`. Update any log monitoring that matches the old text.

- **Upgrade.** Entries that an earlier version added to `banlist.dat` for
  misbehavior are loaded unchanged. They remain strict bans and expire at
  their recorded time, which is at most `-bantime` after they were created.
  They then disappear as usual. To remove them earlier, use `clearbanned` or
  `setban <address> remove`.
