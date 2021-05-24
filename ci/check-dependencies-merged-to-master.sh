#!/bin/sh

self=$(basename $PWD)

fmap() {
        f=$1
        read line
        while test -n "$line"
        do ${f} "$line"
           read line; done
}

## Compute a case pattern that would match local cabal package names.
local_packages="$(find . -name '*.cabal' | grep -v dist-newstyle | fmap basename | sed 's/.cabal//' | xargs echo)"
match_local_packages_case_pattern="$(echo $local_packages | sed 's/ / | /g')"

stack_to_nix_pins=nix/.stack.nix/*.nix
ls ${stack_to_nix_pins} >/dev/null || {
        echo "ERROR: stack-to-nix pins not found: ${stack_to_nix_pins}"
        exit 1
}

for x in ${stack_to_nix_pins}
do
        name=$(basename -s .nix $x)
        eval "case '$name' in 'default' | ${match_local_packages_case_pattern} ) continue; esac"
        url=$(grep 'url =' $x | cut -d'"' -f2 | xargs echo |
              sed s'/^http:/https:/' |
              sed s'/\.git$//')
        rev=$(grep 'rev =' $x | cut -d'"' -f2)
        query="${url}/branch_commits/${rev}"
        echo -n "${name}: "
        if ! curl --silent "${query}" |
                fgrep '<li class="branch">' |
                cut -d'>' -f3 |
                cut -d'<' -f1 | grep 'master'
         then
                cat <<EOF
${name} ${rev} not on master of ${url}
check:
curl --silent ${query}
EOF
                exit 1
        fi
done
