#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034,SC2154,SC2039,SC1007,SC2207,SC2145,SC2155,SC2206
## Don't import this file directly.

## TODO:  debug the spectacular failure this causes..
# set -e

. "${__COMMON_SRCROOT}/scripts/benchmarking/lib-nix.sh"

##
## This depends on the setup done by scripts/common.sh
##

oprint() {
        echo "--( $*" >&2
}
export -f oprint
oprint_top() {
        ## This only prints if ran from the top-level shell process.
        if test -z "${lib_recursing}"; then oprint "$@"; fi
}
export -f oprint_top

vprint() {
        if test -n "${verbose}${debug}"; then echo "-- $*" >&2; fi
}
export -f vprint
vprint_top() {
        ## This only prints if either in debug mode,
        ## or ran from the top-level shell process.
        if test -z "${lib_recursing}" -o -n "${debug}"; then vprint "$@"; fi
}
export -f vprint_top

dprint() {
        if test -n "${debug}"; then echo "-- $*" >&2; fi
}
export -f dprint

fprint() {
        echo "-- FATAL:  $*" >&2
}
export -f fprint

prebuild() {
        local pkg="$1" exe="${2:-${pkg}}"
        vprint "prebuilding the \"$pkg:$exe\" executable in \"${mode}\" mode.."
        run --no-stats "$pkg" "$exe" --help >/dev/null || true
}
export -f prebuild

run() {
        if test -n "${verbose}"
        then run_verbose "$@"
        else run_quiet   "$@"
        fi
}
export -f run

run_verbose() {
        actually_run "$@"
}
export -f run_verbose

run_quiet()
{
        ## This nightmare below (minus the last line) does one simple thing:
        ##   ..it ensures the --build-extra argument has the mode-specific argument
        ##   that muffles output from the mode-appropriate build tool.
        local bld_extra=
        while test -n "$1"
        do case "$1" in
           --build-extra )    bld_extra=$2; shift;;
           * ) break;; esac; shift; done
        case ${mode} in
                nix )               bld_extra="--no-build-output --quiet ${bld_extra}";;
                cabal )             bld_extra="-v0 ${bld_extra}";;
                stack | stack-nix ) bld_extra="--silent ${bld_extra}";; esac;

        actually_run --build-extra "${bld_extra}" "$@"
}
export -f run_quiet

actually_run()
{
        local ARGS=("$@")
        dprint "actually_run:  ${ARGS[@]@Q}"
        local bld_extra=
        local profile= profile_suffix= user_suffix= profile_prefix= profile_file=
        local profile_root='./profile'
        local profmode=
        local rtsopts=
        local stats=t
        local tag=
        while test -n "$1"
        do # ARGS=("$@"); dprint "handling: ${ARGS[@]@Q}"
           case "$1" in
           --build-extra )       bld_extra="$2"; shift;;
           --profile )           profile=$2; shift;;
           --profile-suffix )    user_suffix=.$2; shift;;
           --no-stats )          stats=;;
           --tag ) case $2 in
                           git-head | HEAD ) tag="$(git symbolic-ref HEAD | sed 's,.*/,,g')";;
                           * )               tag=$2;; esac; shift;;
           * ) break;; esac; shift; done
        dprint "actually_run binary extra args:  $*"

        rtsopts="+RTS";
        profile_prefix="${profile_root}/$(generate_mnemonic "${tag}")${user_suffix}"
        case "${profile}" in
           time )            vprint "profiling:  time"
                             profmode='-P';  profile_suffix="prof";;
           space )           vprint "profiling:  space, by default cost centre"
                             profmode='-h';  profile_suffix="hp";;
           space-module )    vprint "profiling:  space, by module"
                             profmode='-hm'; profile_suffix="hp";;
           space-closure )   vprint "profiling:  space, by closure"
                             profmode='-hd'; profile_suffix="hp";;
           space-type )      vprint "profiling:  space, by type"
                             profmode='-hy'; profile_suffix="hp";;
           space-retainer )  vprint "profiling:  space, by retainer"
                             profmode='-hr'; profile_suffix="hp";;
           space-bio )       vprint "profiling:  space, biographic"
                             profmode='-hb'; profile_suffix="hp";
                             rtsopts+=' -N1';;
           '' )              true;;
           * ) fprint "--profile requires a mode argument:  time space space-module space-closure space-type space-retainer space-bio"; exit 1;; esac

        if test -n "${stats}${profile}"
        then mkdir -p "${profile_root}"
        fi
        if test -n "${stats}"
        then rtsopts+=" --machine-readable -t${profile_prefix}.stats"
        fi
        if test -n "${profile}"
        then profile_file="${profile_prefix}${profile_suffix}"
             # rtsopts="+RTS --machine-readable -l -t${profile_prefix}.stats -po${profile_prefix} -ol ${profile_prefix}.eventlog ";
             # Sadly GHC 8.6 doesn't support -ol
             rtsopts+=" -l -po${profile_prefix} ";
        fi
        rtsopts+="${profmode} -RTS";
        if test "${rtsopts}" = "+RTS -RTS"
        then rtsopts=; fi
        vprint "RTS options:    ${rtsopts}"
        if test -n "${rtsopts}"
        then vprint "result prefix:  ${profile_prefix}"; fi

        local pkg="$1"; shift
        local exe="$1"; shift

        case ${mode} in
        nix )       X=(run_nix_executable "$pkg" "$exe" "${bld_extra}" ${rtsopts} "$@");;
        cabal )     X=(cabal v2-run ${bld_extra} exe:$exe --    ${rtsopts} "$@");;
        stack )     X=(stack ${bld_extra} run $exe        --    ${rtsopts} "$@");;
        stack-nix ) X=(stack ${bld_extra} run --nix $exe  --    ${rtsopts} "$@");;
        * ) echo "INTERNAL ERROR: unknown mode:  $mode" >&2; return 1;;
        esac

        vprint "${X[@]}"
        "${X[@]}"
        local status=$?

        if test  -f "${exe}.eventlog"
        then mv "${exe}.eventlog" "${profile_prefix}.eventlog"
        fi
        if test -f "${profile_file}" -a -x "$(command -v profiteur 2>/dev/null)"
        then profiteur "${profile_file}"
        fi
        if test -f "${profile_file}" -a -x "$(command -v profiterole 2>/dev/null)"
        then profiterole "${profile_file}"
        fi
        if test -f "${profile_file}" -a -x "$(command -v eventlog2html 2>/dev/null)"
        then eventlog2html "${profile_prefix}.eventlog"
        fi

        return ${status}
}
export -f actually_run

##
## Misc stuff, too few to be worth splitting out
##
generate_wordpair() {
        case ${mode} in
                nix ) nix-shell -p diceware --run 'diceware --no-caps --num 2 --wordlist en_eff -d-' 2>/dev/null || true;;
                * ) true;; esac
}

generate_mnemonic()
{
        local mnemonic="${1:-$(generate_wordpair)}"
        local timestamp="$(date +%s)"
        local commit="$(git rev-parse HEAD | cut -c-8)"
        local status=''

        if git diff --quiet --exit-code
        then status=pristine
        else status=modified
        fi

        echo "${timestamp}.${commit}.${status}${mnemonic:+.${mnemonic}}"
}
export -f generate_mnemonic
