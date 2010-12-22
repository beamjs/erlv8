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
