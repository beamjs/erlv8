#include "erlv8.hh"

TickHandler(ResultTickHandler) {
 if (((unsigned long) ref) &&
	  (enif_is_identical(array[1],ref))) { // this is our result
	result = term_to_js(vm->env,array[2]);
	return RETURN;
  } else {
   Tick newtick;
   
   newtick.env = enif_alloc_env();
   newtick.tick = enif_make_copy(newtick.env, tick);
   newtick.ref = enif_make_copy(newtick.env, tick_ref);

   vm->pre_pop_ticks.push(newtick);
   
   return DONE;
  }
}
