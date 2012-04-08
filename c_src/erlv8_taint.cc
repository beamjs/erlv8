#include "erlv8.hh"

TickHandler(TaintTickHandler) {
  TRACE("(%p) tain - 1\n", vm->isolate);
  ErlNifEnv *ref_env = enif_alloc_env();
  TRACE("(%p) tain - 2\n", vm->isolate);
  ERL_NIF_TERM taint_ref = enif_make_copy(ref_env, tick_ref);
  TRACE("(%p) tain - 3\n", vm->isolate);
  LHCS(vm->isolate, vm->context);
  TRACE("(%p) tain - 4\n", vm->isolate);
  SEND(vm->server,
       enif_make_tuple3(env,
                        enif_make_atom(env,"result"),
                        enif_make_copy(env,taint_ref),
                        js_to_term(vm->context, vm->isolate, env,term_to_js(vm->context, vm->isolate,vm->env, array[1]))));

  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
