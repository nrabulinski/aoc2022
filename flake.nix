{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Languages for AoC
    # roc = {
    #   url = "github:roc-lang/roc";
    #   flake = false;
    #   inputs = {
    #     nixpkgs.follows = "nixpkgs";
    #     flake-utils.follows = "flake-utils";
    #     rust-overlay.follows = "rust-overlay";
    #   };
    # };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    min-src = {
      url = "github:h3rald/min/v0.37.0";
      flake = false;
    };

    # Build helpers
    crane = {
      url = "github:ipetkov/crane";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
        rust-overlay.follows = "rust-overlay";
      };
    };

    # Common dependencies
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };

    # Other
    nur = {
      url = "github:nrabulinski/nur-packages";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-parts.follows = "flake-parts";
      };
    };
  };

  outputs = {
    self,
    nixpkgs,
    flake-parts,
    nur,
    min-src,
    # roc,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} {
      # Other systems will probably not work
      systems = [
        "x86_64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      perSystem = {
        pkgs,
        inputs',
        system,
        ...
      }: let
        inherit (pkgs) lib;
        inherit (pkgs.stdenv) isDarwin isAarch64 isx86_64;
        pkgs' = import nixpkgs {
          inherit system;
          config.allowBroken = true;
          overlays = [
            nur.overlay
            (final: prev: {
              x86-64 =
                if isx86_64
                then final
                else if isDarwin && isAarch64
                then (import nixpkgs {
                  system = "x86_64-darwin";
                  overlays = [ nur.overlay ];
                })
                else {};
            })
            # (final: prev: {
            #   zig = final.zig_0_10;
            # })
          ];
        };
        # roc' = import roc {
        #   pkgs = pkgs';
        #   cargoSha256 = "sha256-4RzOUC6qve7C/UhSkKYtMIwIvXlOuIraz689vWNJ+b8=";
        # };
      in {
        formatter = pkgs.alejandra;

        devShells.default = pkgs.mkShell {
          packages = with pkgs'; [
            just
            racket
            x86-64.min-lang
            # roc'
          ];

          nativeBuildInputs = with pkgs; [pkg-config];
          buildInputs = lib.optional pkgs.stdenv.isDarwin [pkgs.libiconv];
        };
      };
    };
}
