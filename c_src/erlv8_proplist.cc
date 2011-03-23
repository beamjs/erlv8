#include "erlv8.hh"

TickHandler(ProplistTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM proplist_ref = enif_make_copy(ref_env, tick_ref);

  val_res_t *res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&res))) {
	LHCS(res->ctx);

	v8::Handle<v8::Array> keys = res->val->ToObject()->GetPropertyNames();
	
	ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * keys->Length());
	
	for (unsigned int i=0;i<keys->Length();i++) {
	  v8::Handle<v8::Value> key = keys->Get(v8::Integer::New(i));
	  arr[i] = enif_make_tuple2(vm->env,
								js_to_term(res->ctx,vm->env,v8::Handle<v8::String>::Cast(key)),
								js_to_term(res->ctx,vm->env,res->val->ToObject()->Get(key)));
	}
	ERL_NIF_TERM list = enif_make_list_from_array(vm->env,arr,keys->Length());
	free(arr);
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,proplist_ref),
						  enif_make_copy(env,list)));
  }

  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
