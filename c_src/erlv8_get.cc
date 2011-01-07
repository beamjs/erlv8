#include "erlv8.hh"

TickHandler(GetTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM get_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);
	v8::Local<v8::Value> get_result = obj_res->val->ToObject()->Get(term_to_js(vm->env,array[2]));
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,get_ref),
						  js_to_term(env,get_result)));
  }
  enif_free_env(ref_env);
  return DONE;
}

TickHandler(GetInternalTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM get_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	int index;
	enif_get_int(vm->env, array[2], &index);

	v8::Local<v8::Value> get_result = obj_res->val->ToObject()->GetInternalField(index);
	  
	if (get_result->IsExternal()) {
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,get_ref),
							external_to_term(get_result)));
	} else {
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,get_ref),
							js_to_term(env,get_result)));
	}
	
  }
  enif_free_env(ref_env);
  return DONE;
}
