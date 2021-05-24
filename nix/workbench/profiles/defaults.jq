## Testable with:
##
##   jq -n 'include "defaults" { search: "nix/supervisord-cluster/profiles" }; era_defaults("shelley")'
##
def era_defaults($era):
{ common:
  { era:                              $era

  ## Cluster topology and composition:
  , composition:
    { locations:                      ["LO"]
    , n_bft_hosts:                    1
    , n_singular_hosts:               1
    , n_dense_hosts:                  1
    , dense_pool_density:             1
    , with_observer:                  true
    }

  , genesis:
    ## Trivia
    { network_magic:                  42

    ## Incrementality
    , single_shot:                    true

    ## UTxO & delegation
    , total_balance:                  900000000000000
    , pools_balance:                  800000000000000
    , utxo:                           1000000
    , decentralisation_param:         0

    ## Blockchain time & block density
    , active_slots_coeff:             0.05
    , epoch_length:                   2200   # Ought to be at least (10 * k / f).
    , parameter_k:                    10
    , slot_duration:                  1

    ## Block size & contents
    , max_block_size:                 64000
    , max_tx_size:                    16384

    ## Verbatim:
    , verbatim:
      { protocolParams:
        { poolDeposit: 500000000
        , keyDeposit: 400000
        , rho: 0.0022
        , tau: 0.05
        , a0: 0.3
        , minFeeA: 44
        , minFeeB: 155381
        , decentralisationParam: 0
        }
      }
    }

  , generator:
    { add_tx_size:                    0
    , init_cooldown:                  25
    , inputs_per_tx:                  2
    , outputs_per_tx:                 2
    , tx_fee:                         1000000
    , epochs:                         10
    , tps:                            2
    }

  , node:
    {
    }

  , tolerances:
    { cluster_startup_overhead_s:     60
    , start_log_spread_s:             120
    , last_log_spread_s:              120
    , silence_since_last_block_s:     120
    , tx_loss_ratio:                  0.02
    , finish_patience:                21
    }
  }

, shelley:
  { tolerances:
    { maximum_missed_slots:           0
    }
  }

, allegra:
  {
  }

, mary:
  {
  }
} | (.common * .[$era]);
