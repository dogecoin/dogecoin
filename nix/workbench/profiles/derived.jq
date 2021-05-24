include "defaults";
include "cli-args";

def may_attr($attr; $dict; $defdict; $scale; $suf):
  if ($dict[$attr] //
      error("undefined attr: \($attr)"))
     != $defdict[$attr]
  then [($dict[$attr] | . / $scale | tostring) + $suf] else [] end;

def profile_name($p):
  era_defaults($p.era).genesis     as     $genesis_defaults
| era_defaults($p.era).generator   as   $generator_defaults
| era_defaults($p.era).composition as $composition_defaults
  ## Genesis
| [ "k\($p.composition.n_pools)" ]
  + if $p.composition.n_dense_hosts > 0
    then may_attr("dense_pool_density";
                  $p.composition; $composition_defaults; 1; "ppn")
    else [] end
  + [ ($p.generator.epochs              | tostring) + "ep"
    , ($p.genesis.utxo       | . / 1000 | tostring) + "kU"
    , ($p.genesis.delegators | . / 1000 | tostring) + "kD"
    ]
  + may_attr("tps";
             $p.generator; $generator_defaults; 1; "tps")
  + may_attr("max_block_size";
             $p.genesis;     $genesis_defaults; 1000; "kb")
  + may_attr("add_tx_size";
             $p.generator; $generator_defaults; 1; "b")
  + may_attr("inputs_per_tx";
             $p.generator; $generator_defaults; 1; "i")
  + may_attr("outputs_per_tx";
             $p.generator; $generator_defaults; 1; "o")
  + if $p.composition.with_observer | not
    then ["nobs"]
    else [] end
  | join("-");

def profile_name_era_suffix($era):
  "-\($era | (.[0:2] + .[-2:]))";

def add_derived_params:
  (.genesis.genesis_future_offset //
    if      .composition.n_hosts > 50 then "32 minutes"
    else if .composition.n_hosts == 3 then "3 minutes"
         else "10 minutes" end end)          as $future_offset
| .composition                               as $compo
| .genesis                                   as $gsis
| .generator                                 as $gtor
| .tolerances                                as $tolr
| ($gsis.epoch_length * $gsis.slot_duration) as $epoch_duration
| ($epoch_duration * $gtor.epochs)           as $duration
| (if $compo.dense_pool_density > 1
   then { singular:  $compo.n_singular_hosts
        , dense:     $compo.n_dense_hosts }
   else { singular: ($compo.n_singular_hosts + $compo.n_dense_hosts)
        , dense:     0 }
   end)                                      as $hosts
| $hosts.singular                            as $n_singular_pools
| ($hosts.dense * $compo.dense_pool_density) as $n_dense_pools
| ($n_singular_pools + $n_dense_pools)       as $n_pools

## Note how derivations come in phases, too:
##
| (## First derivation:
   { common:
     { composition:
         { n_hosts:               ($compo.n_bft_hosts + $hosts.singular + $hosts.dense)
         , n_pools:               $n_pools
         , n_singular_hosts:      $hosts.singular
         , n_singular_pools:      $n_singular_pools
         , n_dense_hosts:         $hosts.dense
         , n_dense_pools:         $n_dense_pools
         , n_pool_hosts:          ($hosts.singular + $hosts.dense)
         }
     , genesis:
         { genesis_future_offset: $future_offset
         , delegators:            ($gsis.delegators // $n_pools)
         , pool_coin:             ($gsis.pools_balance / $n_pools | floor)
         , verbatim:
           ## TODO: duplication
           { protocolParams:
             { activeSlotsCoeff:           $gsis.active_slots_coeff
             , epochLength:                $gsis.epoch_length
             , securityParam:              $gsis.parameter_k
             , slotLength:                 $gsis.slot_duration
             , maxTxSize:                  $gsis.max_tx_size
             , protocolParams:
               { "decentralisationParam":  $gsis.decentralisation_param
               , "maxBlockBodySize":       $gsis.max_block_size
               , "nOpt":                   $compo.n_pools
               }
             }
           }
         }
     , generator:
         { tx_count:              ($duration * ([$gtor.tps, 7] | min))
         }
     , node:
         {
         }
     , tolerances:
         { minimum_chain_density: ($gsis.active_slots_coeff * 0.5)
         }
     }
   } | . *
   ## Second derivation:
   { common:
     { genesis:
       { delegator_coin:          ($gsis.pools_balance /
                                   .common.genesis.delegators
                                   | floor)
       }
     }
   })  as $derived
| . * $derived.common
    * ($derived[.era] // {})
| . *
    { name:     ( .era as $era
                | (.name // profile_name(.))
                | . + profile_name_era_suffix($era)
                )
    , cli_args: profile_cli_args(.)
    }
;

def profile_pretty_describe($p):
  [ "Profile: \($p.name)"
  , "  - era:                \($p.era)"
  , "  - epoch slots:        \($p.genesis.epoch_length)"
  , "  - slot duration:      \($p.genesis.slot_duration)"
  , "  - k:                  \($p.genesis.parameter_k)"
  , "  - active slots coeff: \($p.genesis.active_slots_coeff)"
  , "  - hosts:              \($p.composition.n_hosts)"
  , "  - pools:              \($p.composition.n_pools)"
  , "    - normal:             \($p.composition.n_singular_pools)"
  , "    - dense:              \($p.composition.n_dense_pools)"
  , "  - UTxO:               \($p.genesis.utxo)"
  , "  - delegators:         \($p.genesis.delegators)"
  , ""
  ]
  | join("\n");
