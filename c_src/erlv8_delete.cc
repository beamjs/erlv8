#include "erlv8.hh"

TickHandler(DeleteTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM get_ref = enif_make_copy(ref_env, tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);
	v8::Handle<v8::Value> key = term_to_js(ref_env,array[2]);
	if (key->IsString()) {
	  obj_res->val->ToObject()->Delete(key->ToString());
	} else if (key->IsNumber()) {
	  obj_res->val->ToObject()->Delete(key->Uint32Value());
	}
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,get_ref),
						  enif_make_atom(env, "ok")));
  }
  enif_free_env(ref_env);
  return DONE;
}

