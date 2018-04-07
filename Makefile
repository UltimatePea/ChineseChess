t:
	stack build
	stack exec -- chinesechess-exe
p:
	stack build --profile
	stack exec -- chinesechess-exe +RTS -p
