{
  description = "A fixed buffer-size channel, implemented as a ring buffer using Software Transactional Memory (STM)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        haskellPackages = pkgs.haskellPackages;
        
        packageName = "stm-ringbuffer";
        
        # Create a Haskell package using Cabal
        package = haskellPackages.callCabal2nix packageName self {};
        
      in {
        # The built package
        packages.${packageName} = package;
        packages.default = package;
        
        # Development shell with GHC and Cabal
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Haskell development tools
            haskellPackages.ghc
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.ghcid
            
            # Other useful tools
            cabal2nix
          ];
          
          # Set up GHC with the correct package database
          shellHook = ''
            export PS1="\n\[\033[1;32m\][nix-shell:\w]\$\[\033[0m\] "
            echo "Entered the STM RingBuffer development environment"
            echo "Run 'cabal build' to build the project"
          '';
        };
        
        # For compatibility with 'nix develop'
        devShell = self.devShells.${system}.default;
        
        # Add checks to ensure the package builds correctly
        checks.${packageName} = package;
      }
    );
}
