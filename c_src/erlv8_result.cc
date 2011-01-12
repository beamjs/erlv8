#include "erlv8.hh"

TickHandler(ResultTickHandler) {
 if (((unsigned long) ref) &&
	  (enif_is_identical(array[1],ref))) { // this is our result
	result = term_to_js(vm->env,array[2]);
	return RETURN;
  } else {
   return DONE;
  }
}
