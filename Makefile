X64=$(shell file -L `which epmd` | grep x86_64 | wc -l | xargs echo)
X64L=$(shell file -L `which epmd` | grep x86-64 | wc -l | xargs echo)
OSX=$(shell uname | grep Darwin | wc -l | xargs echo)
LINUX=$(shell uname | grep Linux | wc -l | xargs echo)

V8ENV=GYPFLAGS="-f make"
ifeq ($(X64),1)
V8FLAGS=arch=x64
else
V8FLAGS=
endif

ifeq ($(X64L),1)
V8FLAGS=arch=x64
V8ENV=CCFLAGS=-fPIC
endif

ifeq ($(LINUX),1)
ZMQ_FLAGS=--with-pic
else
ZMQ_FLAGS=
endif


all: compile 

deps/v8/.git/config:
	@git submodule init
	@git submodule update

deps/zeromq2/.git/HEAD:
	@git submodule init
	@git submodule update

deps/v8/libv8.a: deps/v8/.git/config 
	cd deps/v8 && $(V8ENV) scons $(V8FLAGS)

deps/zeromq2/src/.libs/libzmq.a: deps/zeromq2/.git/HEAD
	@cd deps/zeromq2 && ./autogen.sh && ./configure $(ZMQ_FLAGS) && make

dependencies: deps/v8/libv8.a deps/zeromq2/src/.libs/libzmq.a

test: compile
	@./rebar eunit skip_deps=true

dbg-test: compile
	@USE_GDB=true ./rebar eunit skip_deps=true

compile: dependencies
	@./rebar get-deps
	@EXTRA_CFLAGS= ./rebar compile

debug: dependencies
	@EXTRA_CFLAGS="-g3 -O0 -DERLV8_DEBUG" ./rebar compile

clean: 
	-rm c_src/*.o

analyze:
	clang --analyze -Xanalyzer "-Ideps/v8/include/" -Xanalyzer "-I/usr/local//Cellar/erlang/R15B/lib/erlang/usr/include"  -Xanalyzer "-Ideps/zeromq2/include/"  c_src/*.cc
