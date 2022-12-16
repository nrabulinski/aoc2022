{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";

    # Languages for AoC
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
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
        nur = inputs'.nur.packages;
        haskellPackages = pkgs.haskell.packages.ghc94;
        haskell = haskellPackages.ghcWithPackages (p: with p; [
          haskell-language-server
          attoparsec
        ]);
        py = pkgs.python3.withPackages (p: with p; [
          z3
        ]);
      in {
        formatter = pkgs.alejandra;

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            just
            racket # Day 1, 2
            nur.min-lang # Day 3, 4
            # TODO: Rust # Day 5, 6
            koka # Day 7, 8
            openjdk19 # Day 9, 10
            scala_3 # Day 11, 12
            haskell # Day 13, 14, 15p1
            py # Day 15p2
            
            nur.klong # I'll get to it eventually...
          ];

          nativeBuildInputs = with pkgs; [pkg-config pcre2.dev];
          buildInputs = lib.optional pkgs.stdenv.isDarwin [pkgs.libiconv];
        };
      };
    };
}
