dev:
	ghcid --lint --command "cabal repl ensemble" --output ghcid.txt

hoogle:
	nix-shell shell-hoogle.nix --run "hoogle server --local"
