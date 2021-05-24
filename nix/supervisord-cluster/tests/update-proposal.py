import sys
import time

from base import cluster

cluster.submit_update_proposal([("decentralization-parameter", "0.5")])
print(f"Update Proposal submited. Sleeping until next epoch")
cluster.sleep_until_next_epoch()
cluster.cli.refresh_pparams()
d = cluster.cli.pparams["decentralisationParam"]
if d == 0.5:
    print("Cluster update proposal successful!")
    sys.exit(0)
else:
    print(f"Cluster update proposal failed! Expected 0, got {d}")
    print(cluster.get_tip())
    sys.exit(1)
