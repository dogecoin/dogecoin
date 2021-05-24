############################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs
, project
, exes
, platform
}:

let
  lib = pkgs.lib;
  name = "cardano-node-${project.version}-${platform}";

in pkgs.runCommand name {
    buildInputs = with pkgs.buildPackages; [
      zip
      haskellBuildUtils.package
    ];
  } ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes} ./
  mkdir -p configuration
  cp -Rv ${../configuration}/* ./configuration/
  chmod -R +w .

  ${if (platform == "win64")
    then "zip -r $out/${name}.zip ."
    else "tar -czf $out/${name}.tar.gz ."
  }
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
