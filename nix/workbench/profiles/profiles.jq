## Profiles are named bundles of parameters, where parameters are
## classified into sections:
##
##  - genesis
##  - generator
##  - node
##  - tolerances (ranges of acceptable properties, given above parameters)
##
## When combined with cluster composition (an extract from topology, see the
## definition below) and service parameters (non-crucial things like state directory,
## port ranges, etc), profiles ought to completely specify cluster parameters,
## allowing genesis and all necessary configuration files to be generated.
##
## Profiles themselves are layered as follows:
##
##  - era-dependent defaults for the aforementioned sections:
##    - profiles/defaults.jq
##
##  - overlayed with generated profile variants + ad-hoc profiles:
##    - profiles/variants.jq and profiles/adhoc.jq
##
##  - each then further overlayed with derived parameters, computed from the above:
##    - profiles/derived.jq
##
## Profiles variants are generated as a cartesian product of variations
## of the three axes: genesis, generator and node.
## These generated profiles are assigned computed names, as per the
## profile_name() function in 'profiles/derived.jq'.
##
## Cluster composition must have the following structure:
##  { n_bft_hosts:      INT
##  , n_singular_hosts: INT
##  , n_dense_hosts:    INT
##  }
##
## Names of non-adhoc profiles are computed by the 'profile_name' function in
## profiles/derived.jq, with era suffix appended, and have the following structure:
##
##   k${n_pools}
##    -[${dense_pool_density}ppn]
##    -${epochs}ep
##    -${utxo}kU
##    -${delegators}kD
##    -[${tps}tps]
##    -[${max_block_size}blk]
##    -[${add_tx_size}b]
##    -[${inputs_per_tx}i]
##    -[${outputs_per_tx}o]
##
## ..where [] denote optionals, that are only included if the profile
##   deviates from profile defaults.
##
## Testable by:
##
##   nix-build -A profiles       ## or simply:  make profiles
##
## ..which simply calls ./profiles.nix with {} params.
##

include "topology";
include "defaults";
include "adhoc";
include "variants";
include "derived";

def compute_profiles($era; $mcompo; $topo; $extra_profiles):
    ($mcompo // topology_composition($topo // {}) // {}) as $compo

  ## Profiles are variants + custom (or aux) profiles:
  | all_profile_variants + adhoc_profiles + $extra_profiles

  | map (## Each profile extends defaults:
         era_defaults($era) * .

         ## Profiles can define their own cluster composition.
         | . * { composition: (.composition // $compo) }

         ## Compute the derived params.
         | add_derived_params
        );

def profiles($era; $mcompo; $topo; $extra_profiles):
  compute_profiles($era; $mcompo; $topo; $extra_profiles)
  | map (## Assemble into a dictionary..
           { "\(.name)":
               ## ..and cleanup:
               . | delpaths ([["generator", "epochs"]])})
  | add;

def profile_names($era; $mcompo; $topo; $extra_profiles):
  compute_profiles($era; $mcompo; $topo; $extra_profiles)
  | map (.name);

def has_profile($era; $mcompo; $topo; $extra_profiles; $name):
  compute_profiles($era; $mcompo; $topo; $extra_profiles)
  | map (.name == $name)
  | any;
