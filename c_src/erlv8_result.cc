#include "erlv8.hh"

TickHandler(ResultTickHandler) {
 if (((unsigned long) ref) &&
	  (enif_is_identical(array[1],ref))) { // this is our result
	result = term_to_js(vm->env,array[2]);
	return RETURN;
  } else {
   SEND(vm->server,
		enif_make_tuple3(env,
						 enif_make_atom(env,"result"),
						 enif_make_copy(env,array[1]),
						 enif_make_copy(env,array[2])));
   return DONE;
  }
}
