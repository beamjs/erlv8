#include "erlv8.hh"

TickHandler(SetTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM set_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	v8::PropertyAttribute property_attribute = v8::None;

	if (arity == 5) {
	  property_attribute = term_to_property_attribute(vm->env,array[4]);
	}
	
	obj_res->val->ToObject()->Set(term_to_js(vm->env,array[2]),term_to_js(vm->env,array[3]), property_attribute);
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,set_ref),
						  enif_make_copy(env,array[3])));
  } 
  enif_free_env(ref_env);
  return DONE;
}

TickHandler(SetInternalTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM set_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *obj_res;
  char name[MAX_ATOM_LEN];
  unsigned len;

  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	int index;
	enif_get_int(vm->env, array[2], &index);

	if (!strcmp(tick_name,"set_internal_extern")) {
	  enif_get_atom_length(vm->env, array[4], &len, ERL_NIF_LATIN1);
	  enif_get_atom(vm->env,array[4],(char *)&name,len + 1, ERL_NIF_LATIN1);
	  v8::Handle<v8::Object> proto = extern_name_to_proto(vm, name);
	  obj_res->val->ToObject()->SetInternalField(index,term_to_external(array[3]));
	} else {
	  obj_res->val->ToObject()->SetInternalField(index,term_to_js(vm->env,array[3]));
	}

	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,set_ref),
						  enif_make_copy(env,array[3])));
  } 
  enif_free_env(ref_env);
  return DONE;
}
