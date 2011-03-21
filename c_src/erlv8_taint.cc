#include "erlv8.hh"

TickHandler(TaintTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM taint_ref = enif_make_copy(ref_env, tick_ref);
  LHCS(vm->context);
  
  SEND(vm->server,
       enif_make_tuple3(env,
                        enif_make_atom(env,"result"),
                        enif_make_copy(env,taint_ref),
                        js_to_term(env, term_to_js(vm->env, array[1]))));

  enif_free_env(ref_env);
  return DONE;
}
