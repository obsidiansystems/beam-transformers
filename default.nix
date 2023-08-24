{
  reflex-platform ? import ./dep/reflex-platform { },
  nix ? import ./dep/nix
}: (reflex-platform.project ({ pkgs, thunkSource, ... }: {
  name = "beam-transformers";
  src = ./.;
  compiler-nix-name = "ghc8107";
  shells = p: with p; [ beam-transformers-backend beam-transformers-common ];
  shellTools = {
    cabal-install = "3.2.0.0";
    haskell-language-server = "1.8.0.0";
  };
})).extend (self: super: {
  inherit nix;
  shells = super.shells // {
    ghc = self.shell-driver {
      exactDeps = false;
      additional = p: [];
      buildInputs = [ super.pkgs.ghcid ];
    };
  };
})
