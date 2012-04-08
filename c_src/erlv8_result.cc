#include "erlv8.hh"

TickHandler(ResultTickHandler) {
  v8::HandleScope handle_scope;
  TickHandlerResolution result;
  
  if (((unsigned long) ref) &&
      (enif_is_identical(array[1],ref))) { // this is our result
    result.value = handle_scope.Close(term_to_js(vm->context, vm->isolate, vm->env,array[2]));
    result.type = RETURN;
    return result;
  } else {
    Tick newtick;
    
    newtick.env = enif_alloc_env();
    newtick.tick = enif_make_copy(newtick.env, tick);
    newtick.ref = enif_make_copy(newtick.env, tick_ref);
    
    vm->pop_ticks.push(newtick);

    result.type = DONE;    
    return result;
  }
}
