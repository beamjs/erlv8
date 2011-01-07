#include "erlv8.hh"

TickHandler(UnknownTickHandler) {
  SEND(vm->server,
	   enif_make_tuple2(env,
						enif_make_atom(env,"retick"),
						enif_make_copy(env,tick_ref)));
  return DONE;
}

