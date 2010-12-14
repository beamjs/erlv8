X64=$(shell file -L `which epmd` | grep x86_64 | wc -l | xargs echo)


ifeq ($(X64),1)
V8FLAGS=arch=x64
else
V8FLAGS=
endif

all: compile 

deps/v8/.git/config:
	@git submodule init
	@git submodule update

deps/v8/libv8.a: deps/v8/.git/config
	@cd deps/v8 && scons $(V8FLAGS)

dependencies: deps/v8/libv8.a

test: compile
	@./rebar eunit

compile: dependencies
	@./rebar compile
