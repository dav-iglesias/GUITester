all: compile

compile:
	@erl -make

clean:
	-rm -f ebin/*.beam src/*~ doc/* erl_crash.dump *~

run: compile
	erl -sname shell -pa ebin ../Visualizing-EUnit-tests/ebin
