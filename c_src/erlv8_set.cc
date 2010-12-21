#include "erlv8.hh"

TickHandler(SetTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM set_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);
	obj_res->val->ToObject()->Set(term_to_js(vm->env,array[2]),term_to_js(vm->env,array[3]));
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,set_ref),
						  enif_make_copy(env,array[3])));
  } 
  enif_free_env(ref_env);
  return DONE;
}
