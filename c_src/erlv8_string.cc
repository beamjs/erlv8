#include "erlv8.hh"

TickHandler(ToStringTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM to_string_ref = enif_make_copy(ref_env, tick_ref);
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,to_string_ref),
						js_to_term(vm->context,env,term_to_js(vm->context,vm->env,array[1])->ToString())));
  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
};

TickHandler(ToDetailStringTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM to_string_ref = enif_make_copy(ref_env, tick_ref);
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,to_string_ref),
						js_to_term(vm->context,env,term_to_js(vm->context,vm->env,array[1])->ToDetailString())));
  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
};

