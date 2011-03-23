#include "erlv8.hh"

TickHandler(InternalCountTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM get_ref = enif_make_copy(ref_env, tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);
	
	int ifc = obj_res->val->ToObject()->InternalFieldCount();
	
	SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,get_ref),
							enif_make_int(env, ifc)));
  } 
  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
