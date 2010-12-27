#include "erlv8.hh"

TickHandler(RetickTickHandler) {
  SEND(vm->server,
	   enif_make_tuple2(env,
						enif_make_atom(env,"retick"),
						enif_make_copy(env,vm->tick_ref)));
  return DONE;
}

