#include "erlv8.hh"

TickHandler(ListTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM list_ref = enif_make_copy(ref_env, tick_ref);

  val_res_t *res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&res))) {
	LHCS(res->ctx);

	if (res->val->IsArray()) {
	  v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(res->val->ToObject());
	  
	  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
	  for (unsigned int i=0;i<array->Length();i++) {
		arr[i] = js_to_term(res->ctx,vm->env,array->Get(v8::Integer::NewFromUnsigned(i)));
	  }
	  ERL_NIF_TERM list = enif_make_list_from_array(vm->env,arr,array->Length());
	  free(arr);
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,list_ref),
							enif_make_copy(env,list)));
	}
  }

  enif_free_env(ref_env);
  return DONE;
}
