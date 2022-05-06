## Very Much Frequently Asked Questions ❓

### How much doge can exist? – So many puppies! 🐕
Early 2015 (approximately a year and a half after release) there were
approximately 100,000,000,000 coins.
Each subsequent block will grant 10,000 coins to encourage miners to continue to
secure the network and make up for lost wallets on hard drives/phones/lost
encryption passwords/etc.


### Such mining information ⛏

Dogecoin uses a simplified variant of the scrypt key derivation function as its
proof of work with a target time of one minute per block and difficulty
readjustment after every block. The block rewards are fixed and halve every
100,000 blocks. Starting with the 600,000th block, a permanent reward of
10,000 Dogecoin per block will be issued.

Originally, a different payout scheme was envisioned with block rewards being
determined by taking the maximum reward as per the block schedule and applying
the result of a Mersenne Twister pseudo-random number generator to arrive at a
number between 0 and the maximum reward.

This was changed starting with block 145,000, to prevent large pools from gaming
the system and mining only high reward blocks. At the same time, the difficulty
retargeting was also changed from four hours to once per block (every minute),
implementing an algorithm courtesy of the DigiByte Coin development team, to
lessen the impact of sudden increases and decreases of network hashing rate.

**The current block reward schedule:**

| Block                | Reward in Dogecoin |
| :------------------- | -----------------: |
| 1–99,999             |        0–1,000,000 |
| 100,000–144,999      |          0–500,000 |
| 145,000–199,999      |            250,000 |
| 200,000–299,999      |            125,000 |
| 300,000–399,999      |             62,500 |
| 400,000–499,999      |             31,250 |
| 500,000–599,999      |             15,625 |
| 600,000+             |             10,000 |

**The original block reward schedule, with one-minute block targets and
four-hour difficulty readjustment:**

| Block                | Reward in Dogecoin |
| :------------------- | -----------------: |
| 1–99,999             |        0–1,000,000 |
| 100,000–199,999      |          0–500,000 |
| 200,000–299,999      |          0–250,000 |
| 300,000–399,999      |          0–125,000 |
| 400,000–499,999      |           0–62,500 |
| 500,000–599,999      |           0–31,250 |
| 600,000+             |             10,000 |

### what's the difference bertween dogecoin core and multi-doge?

Core: The go to standard, can do anything and everything. By running core you are running a dogecoin node (if you forward port 22556 on your router) and contributing to the decentralized consensus mechanism that makes dogecoin work. You don't need to trust others that incoming transactions are legit because you verify them yourself. Takes a lot of bandwidth, memory, and hard drive space to run.

Multidoge: A powerful light wallet that offers most of the important core functionality, such as backups and importing/exporting private keys. Can not be used for large transactions with a lot of inputs/outputs (such as mining or faucets payouts), may crash the client. Doesn't need to download the full dogecoin blockchain because it depends on other people running dogecoin core to verify transactions. Takes little bandwidth, memory and hard drive space to run.
