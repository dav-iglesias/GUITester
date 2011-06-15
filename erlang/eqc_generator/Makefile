all: compile

compile:
	@erl -make

clean:
	-rm -f ebin/*.beam src/*~ doc/* erl_crash.dump *~

run: compile
	erl -sname shell -pa ebin ../Visualizing-EUnit-tests/ebin

doc: src/parser.erl src/eqc_generator.erl
	erl -noshell -eval "edoc:files([\"src/parser.erl\"], [{dir, \"doc/\"}, {private, true}])" -s init stop
