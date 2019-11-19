all: templation/templation.cabal nix/templation.nix

templation/templation.cabal: templation/package.yaml
	cd templation && hpack

nix/templation.nix: templation/package.yaml
	cd nix && cabal2nix ../templation > templation.nix
