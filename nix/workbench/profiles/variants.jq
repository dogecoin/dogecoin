## Profile variants are defined as a cartesian product of
## variations of genesis/generator/node axes.

def genesis_profile_variants:

  ## Baseline:
  [ { genesis: { utxo: 2000000, delegators:  500000 } }

  ## Baseline, tweaked for fast local repro:
  , { genesis: { utxo: 2000000, delegators:  500000
               , slot_duration: 0.2 }
    , composition: { with_observer: false }}

  ## Size-varied derivatives of baseline:
  , { genesis: { utxo: 2000000, delegators:  750000 } }
  , { genesis: { utxo: 2000000, delegators: 1000000 } }
  , { genesis: { utxo: 3000000, delegators:  500000 } }
  , { genesis: { utxo: 4000000, delegators:  500000 } }
  , { genesis: { utxo: 4000000, delegators: 1000000 } }
  , { genesis: { utxo: 2000000, delegators:  500000, dense_pool_density: 10 } }
  , { genesis: { utxo: 2000000, delegators:  500000, dense_pool_density: 20 } }

  ## TPS-varied derivatives of baseline:
  , { genesis: { utxo: 2000000, delegators:  500000 }
    , generator: { tps: 5 } }
  , { genesis: { utxo: 2000000, delegators:  500000 }
    , generator: { tps: 10 } }
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 5 } }
  , { genesis: { utxo: 4000000, delegators: 1000000 }
    , generator: { tps: 10 } }
  ];

def generator_profile_variants:
  [ { generator: {} }
  ];

def node_profile_variants:
  [ { node: {} }
  ];

def       all_profile_variants:
  [   genesis_profile_variants
  , generator_profile_variants
  ,      node_profile_variants
  ]
  | [combinations]
  | map (reduce .[] as $item ({}; . * $item));
