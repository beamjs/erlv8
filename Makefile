all: compile 

test: compile
	@./rebar eunit

compile:
	@./rebar compile
