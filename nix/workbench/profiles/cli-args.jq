def profile_cli_args($p):
{ common:
  { createSpec:
    [ "--supply",                  $p.genesis.total_balance
    , "--testnet-magic",           $p.genesis.network_magic
    , "--gen-genesis-keys",        $p.composition.n_bft_hosts
    , "--gen-utxo-keys",           1
    ]
 , createFinalIncremental:
    ([ "--supply",                 ($p.genesis.total_balance -
                                    $p.genesis.pools_balance)
     , "--gen-utxo-keys",          1
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [  ]
     else [] end)
  , createFinalBulk:
    ([ "--supply",                 ($p.genesis.total_balance -
                                    $p.genesis.pools_balance)
     , "--gen-utxo-keys",          1
     , "--gen-genesis-keys",       $p.composition.n_bft_hosts
     , "--supply-delegated",       $p.genesis.pools_balance
     , "--gen-pools",              $p.composition.n_pools
     , "--gen-stake-delegs",       ([ $p.composition.n_pools
                                    , $p.genesis.delegators ]
                                     | max)
     , "--testnet-magic",          $p.genesis.network_magic
     , "--num-stuffed-utxo",       ($p.genesis.utxo - $p.genesis.delegators - 1)
                                   ## 1 is for the generator's very own funds.
     ] +
     if $p.composition.dense_pool_density != 1
     then
     [ "--bulk-pool-cred-files",   $p.composition.n_dense_hosts
     , "--bulk-pools-per-file",    $p.composition.dense_pool_density ]
     else [] end)
  , pools:
    [ "--argjson"
    , "initialPoolCoin",           $p.genesis.pool_coin
    ]
  }
}
| .common * (.[$p.era] // {})
;
