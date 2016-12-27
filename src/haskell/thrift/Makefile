all:	
	@(thrift -r -o . --gen hs ../../thrift/poets.thrift)
	@(./fixthrifthaskellcode)
	@(cabal configure)
	@(cabal install)
clean:
	@(rm -rf gen-hs)
	@(cabal clean)