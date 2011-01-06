#include "erlv8.hh"

TickHandler(ToStringTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM to_string_ref = enif_make_copy(ref_env, vm->tick_ref);
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,to_string_ref),
						js_to_term(env,term_to_js(vm->env,array[1])->ToString())));
  enif_free_env(ref_env);
  return DONE;
};

TickHandler(ToDetailStringTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM to_string_ref = enif_make_copy(ref_env, vm->tick_ref);
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,to_string_ref),
						js_to_term(env,term_to_js(vm->env,array[1])->ToDetailString())));
  enif_free_env(ref_env);
  return DONE;
};

