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
    # roc,
    ...
  }:
    flake-parts.lib.mkFlake {inherit self;} {
      systems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];

      perSystem = {pkgs, inputs', system, ...}: let
        inherit (pkgs) lib;
        pkgs' = import nixpkgs {
          inherit system;
          config.allowBroken = true;
          overlays = [
            nur.overlay
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
          packages = [
            pkgs.just
            pkgs'.racket
            # roc'
          ];

          nativeBuildInputs = with pkgs; [ pkg-config ];
          buildInputs = lib.optional pkgs.stdenv.isDarwin [ pkgs.libiconv ];
        };
      };
    };
}
