all:
	make -C modes/haskell-mode
	cd pac && cabal build
