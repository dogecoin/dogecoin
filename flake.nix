{
  description = "Cardano Node";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    customConfig = {
      url = "path:./custom-config";
    };
  };

  outputs = { self, nixpkgs, utils, haskellNix, iohkNix, customConfig }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) head systems mapAttrs recursiveUpdate mkDefault
        getAttrs optionalAttrs nameValuePair attrNames;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          customConfig = recursiveUpdate
            (import ./custom-config final.customConfig)
            customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // iohkNix.lib
            // final.cardanoLib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        inherit (pkgs.commonLib) eachEnv environments;

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.cardanoNodeProject.flake {};

        muslFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.musl64;
        }).cardanoNodeProject.flake {};

        windowsFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.mingwW64;
        }).cardanoNodeProject.flake {};

        scripts = flattenTree pkgs.scripts;

        checkNames = attrNames flake.checks;

        checks =
          # Linux only checks:
          optionalAttrs (system == "x86_64-linux") (
            prefixNamesWith "windows/" (removeAttrs
              (getAttrs checkNames windowsFlake.checks)
              ["cardano-node-chairman:test:chairman-tests"]
            )
            // (prefixNamesWith "nixosTests/" (mapAttrs (_: v: v.${system} or v) pkgs.nixosTests))
          )
          # checks run on default system only;
          // optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (pkgs.cardanoNodeProject.projectModule) src;
            };
          };

        exes = collectExes flake.packages;
        exeNames = attrNames exes;
        lazyCollectExe = p: getAttrs exeNames (collectExes p);

        packages = {
          inherit (devShell) devops;
          inherit (pkgs) cardano-node-profiled cardano-node-eventlogged cardano-node-asserted;
        }
        // scripts
        // exes
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (lazyCollectExe
                (if system == "x86_64-darwin" then flake else muslFlake).packages)))
        # Linux only packages:
        // optionalAttrs (system == "x86_64-linux") (
          prefixNamesWith "windows/" (lazyCollectExe windowsFlake.packages)
          // {
            "dockerImage/node" = pkgs.dockerImage;
            "dockerImage/submit-api" = pkgs.submitApiDockerImage;
          }
        )
        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" checks);

      in recursiveUpdate flake {

        inherit environments packages checks;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-node:exe:cardano-node";

        # Run by `nix run .`
        defaultApp = flake.apps."cardano-node:exe:cardano-node";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
          cardano-ping = { type = "app"; program = pkgs.cardano-ping.exePath; };
        }
        # nix run .#<exe>
        // (collectExes flake.apps);
      }
    ) // {
      overlay = import ./overlay.nix self;
      nixosModules = {
        cardano-node = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-node-service.nix ];
          services.cardano-node.cardanoNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
        cardano-submit-api = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-submit-api-service.nix ];
          services.cardano-submit-api.cardanoNodePkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
