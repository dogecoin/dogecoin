#!/usr/bin/env bash
# shellcheck disable=SC2039,SC2154,SC2206,SC2155,SC2086,SC2034
## Citing the bash BUGS manpage section:
## > Array variables may not (yet) be exported
run_nix_executable() {
        local pkg="$1" name="$2" extra="$3"
        shift 3
        local cache_var=nix_executable_cache_${pkg/-/_}_${name/-/_}
        if test -z "${!cache_var}"
        then dprint "executable cache miss for \"${pkg}:${name}\": ${!nix_executable_cache_@}"
             fill_nix_executable_cache_entry "${pkg}" "${name}" "${extra}"
        else dprint "executable cache hit for \"$${pkg}:{name}\""
        fi
        dprint "${!cache_var} ${*@Q}"
        ${!cache_var} "$@"
}

fill_nix_executable_cache_entry() {
        local pkg="$1" name="$2" extra="$3"
        local nixattr='haskellPackages.'${pkg}'.components.exes.'${name}
        vprint "filling the Nix executable cache for \"$pkg:$name\".."
        NIX_BUILD=(
                nix-build
                "${__COMMON_SRCROOT}/default.nix"
                --no-out-link
                ${extra}
                ${defaultnix_args}
                -A "${nixattr}"
        )
        dprint "${NIX_BUILD[*]}"
        local out=$("${NIX_BUILD[@]}")
        if test -z "${out}"
        then fprint "failed to build ${nixattr}, rerun with --verbose"
             local __QUOTED="${NIX_BUILD[*]@Q}"
             local __FILTERED=${__QUOTED/" '--no-build-output'"}
             fprint "or:  ${__FILTERED/\" '--quiet'\"}"
             exit 1
        fi
        local cache_var=nix_executable_cache_${pkg/-/_}_${name/-/_}
        eval export ${cache_var}
        declare -n nix_cache_write_ref=${cache_var}
        nix_cache_write_ref=${out}/bin/${name}
        vprint "cached ${name} = ${!cache_var}"
        dprint "new cache:  ${!nix_executable_cache_@}"
}
