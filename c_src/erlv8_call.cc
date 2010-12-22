#include "erlv8.hh"

TickHandler(CallTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM call_ref = enif_make_copy(ref_env, vm->tick_ref);
  val_res_t *fun_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&fun_res))) {
	ERL_NIF_TERM head, tail;
	ERL_NIF_TERM current = array[2];
	unsigned int alen;
	
	enif_get_list_length(vm->env,array[2],&alen);
	
	v8::Local<v8::Value> *args = NULL;
	args = new v8::Local<v8::Value>[alen];
	int i = 0;
	while (enif_get_list_cell(vm->env, current, &head, &tail)) {
	  args[i] = v8::Local<v8::Value>::New(term_to_js(vm->env,head));
	  i++; current = tail;
	}
	v8::Handle<v8::Object> recv;
	if (arity == 4) { // this is specified
	  recv = term_to_js(vm->env, array[3])->ToObject();
	} else {
	  recv = fun_res->ctx->Global();
	}
	v8::TryCatch try_catch;
	v8::Local<v8::Value> call_result = v8::Handle<v8::Function>::Cast(fun_res->val)->Call(recv, alen, args);
	if (call_result.IsEmpty()) {
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,call_ref),
							enif_make_tuple2(env,
											 enif_make_atom(env,"throw"),
											 enif_make_tuple2(env,
															  enif_make_atom(env,"error"),
															  js_to_term(env,try_catch.Exception())))));
	} else {
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,call_ref),
							js_to_term(env,call_result)));
	}
	delete [] args;
	args = NULL;
  }
  enif_free_env(ref_env);
  return DONE;		
}
