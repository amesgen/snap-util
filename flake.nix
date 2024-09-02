{
  inputs = {
    haskellNix = {
      url = "github:input-output-hk/haskell.nix";
      inputs.stackage.url = "github:input-output-hk/empty-flake";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import inputs.nixpkgs {
        inherit system;
        inherit (inputs.haskellNix) config;
        overlays = [ inputs.haskellNix.overlay ];
      };
      inherit (pkgs) lib;
      project = pkgs.haskell-nix.cabalProject' {
        src = ./.;
        compiler-nix-name = "ghc96";
      };
      get-snap-util-exe = hsPkgs: hsPkgs.snap-util.components.exes.snap-util;
    in
    {
      packages = {
        default = get-snap-util-exe project.hsPkgs;
      } // lib.optionalAttrs (system == "x86_64-linux") {
        musl = get-snap-util-exe project.projectCross.musl64.hsPkgs;
      };
      legacyPackages = { inherit project; };
      devShells.default = project.shellFor {
        tools.cabal = { };
        withHoogle = false;
      };
    }
  );
  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
      "https://cache.zw3rk.com"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "loony-tools:pr9m4BkM/5/eSTZlkQyRt57Jz7OMBxNSUiMC4FkcNfk="
    ];
  };
}
