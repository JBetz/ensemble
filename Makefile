dev:
	ghcid --lint --command "cabal repl lib:ensemble" --output ghcid.txt

hoogle:
	nix-shell shell-hoogle.nix --run "hoogle server --local"

build-windows-executable:
	$(eval OUT := $(shell nix-build -A projectCross.mingwW64.hsPkgs.ensemble.components.exes.ensemble --no-out-link))
	cp -r ${OUT}/bin ${DESTINATION}