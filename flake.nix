{
  description = "monad-schedule";

  nixConfig = {
    extra-substituters = [
      "https://rhine.cachix.org"
    ];
    extra-trusted-public-keys = [
      "rhine.cachix.org-1:oFsONI6lXn3XG4aVmIURDa2Rn0dW5XTPy6eJWROIs8k="
    ];
  };

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable-small";
  };

  outputs = inputs:
    with builtins;
    let
      lib = inputs.nixpkgs.lib;

      # All GHC versions that this project is tested with.
      # To be kept in sync with the `tested-with:` section in monad-schedule.cabal.
      # To do: Automated check whether this is the same as what get-tested returns.
      # Currently blocked on https://github.com/Kleidukos/get-tested/issues/39
      supportedGhcs = [
        "ghc92"
        "ghc94"
        "ghc96"
        "ghc98"
        # "ghc910" # nixpkgs support isn't good enough yet, uncomment as soon as nixpkgs has moved
      ];

      # The Haskell packages set, for every supported GHC version
      hpsFor = pkgs:
        lib.genAttrs supportedGhcs (ghc: pkgs.haskell.packages.${ghc})
        // { default = pkgs.haskellPackages; };

      # A haskellPackages overlay containing everything defined in this repo
      haskellPackagesOverlay = hfinal: hprev: {
        monad-schedule = hfinal.callCabal2nix "monad-schedule" ./. { };
      };

      # A nixpkgs overlay containing everything defined in this repo, for reuse in downstream projects
      overlay = final: prev:
        let
          hps = hpsFor final;

          # Overrides that are necessary because of dependencies not being up to date or fixed yet in nixpkgs.
          # Check on nixpkgs bumps whether some of these can be removed.
          temporaryHaskellOverrides = with prev.haskell.lib.compose; [
            (hfinal: hprev: {
              time-domain = doJailbreak hprev.time-domain;
            })
          ];
        in
        {
          # The Haskell package set containing the packages defined in this repo
          haskell = prev.haskell // {
            packageOverrides = lib.composeManyExtensions ([
              prev.haskell.packageOverrides
              haskellPackagesOverlay
            ]
            ++ temporaryHaskellOverrides
            );
          };

          monad-schedule-all = prev.symlinkJoin {
            name = "monad-schedule-all";
            paths = map (hp: hp.monad-schedule) (attrValues hps);
          };
        };

      # Helper to build a flake output for all systems that are defined in nixpkgs
      forAllPlatforms = f:
        mapAttrs (system: pkgs: f system (pkgs.extend overlay)) inputs.nixpkgs.legacyPackages;
    in
    {
      # Reexport the overlay so other downstream flakes can use it to develop monad-schedule projects with low effort.
      overlays.default = overlay;

      # Usage: nix fmt
      formatter = forAllPlatforms (system: pkgs: pkgs.nixpkgs-fmt);

      # This builds all monad-schedule packages on all GHCs, as well as docs and sdist
      # Usage:
      # - nix build
      # - nix build .#monad-schedule-all
      packages = forAllPlatforms (system: pkgs: {
        default = pkgs.monad-schedule-all;
      });

      # We re-export the entire nixpkgs package set with our overlay.
      # Usage examples:
      # - nix build .#haskellPackages.monad-schedule
      # - nix build .#haskell.packages.ghc98.monad-schedule
      legacyPackages = forAllPlatforms (system: pkgs: pkgs);

      # Usage: nix develop (will use the default GHC)
      # Alternatively, specify the GHC: nix develop .#ghc98
      devShells = forAllPlatforms (systems: pkgs: mapAttrs
        (_: hp: hp.shellFor {
          packages = ps: [ ps.monad-schedule ];
          nativeBuildInputs = with hp; [
            cabal-gild
            cabal-install
            fourmolu
            haskell-language-server
          ];
        })
        (hpsFor pkgs));
    };
}
