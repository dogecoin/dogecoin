"""Usage: cluster-test.py [--network-magic=INT] [--state-dir=DIR] [--num-delegs=INT]

Options:
    -n, --network-magic <magic>  network magic [default: 42]
    -d, --num-delegs <int>  number of delegators [default: 1]
    -s, --state-dir <dir>  state directory [default: "./state-cluster-test"]
"""

from docopt import docopt
from cardanolib import CardanoCluster,CardanoCLIWrapper

arguments = docopt(__doc__)
cluster = CardanoCluster(int(arguments['--network-magic']), arguments["--state-dir"], int(arguments['--num-delegs']), "shelley")
cluster.cli.refresh_pparams()
