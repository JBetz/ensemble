dev:
	ghcid --lint --command "cabal repl lib:ensemble" --output ghcid.txt

hoogle:
	hoogle generate --local > /dev/null && hoogle server -p 8080 --local > /dev/null
