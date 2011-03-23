#include "erlv8.hh"

TickHandler(ScriptTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM script_ref = enif_make_copy(ref_env, tick_ref);
  unsigned len;
  enif_get_list_length(vm->env, array[2], &len);
  char * buf = (char *) malloc(len + 1);
  enif_get_string(vm->env,array[2],buf,len + 1, ERL_NIF_LATIN1);

  ctx_res_t *res;
  if (enif_get_resource(vm->env,array[1],ctx_resource,(void **)(&res))) {
	v8::Context::Scope context_scope(res->ctx);
	

	v8::TryCatch try_catch;
	
	v8::ScriptOrigin * origin = new v8::ScriptOrigin(term_to_js(res->ctx,vm->env,array[3])->ToString(),
													 term_to_js(res->ctx,vm->env,array[4])->ToInteger(),
													 term_to_js(res->ctx,vm->env,array[5])->ToInteger());
	
	v8::Handle<v8::String> script = v8::String::New(buf, len);
	v8::Handle<v8::Script> compiled = v8::Script::Compile(script,origin);
	
	delete origin;
	
	if (compiled.IsEmpty()) {
	  SEND(vm->server,enif_make_tuple3(env,
									   enif_make_atom(env,"result"),
									   enif_make_copy(env, script_ref),
									   enif_make_tuple2(env,
														enif_make_atom(env,"throw"),
														js_to_term(res->ctx,env,try_catch.Exception()))));
	} else {
	  v8::Handle<v8::Value> value = compiled->Run();
	  if (value.IsEmpty()) {
		SEND(vm->server,enif_make_tuple3(env,
										 enif_make_atom(env,"result"),
										 enif_make_copy(env, script_ref),
										 enif_make_tuple2(env,
														  enif_make_atom(env,"throw"),
														  js_to_term(res->ctx,env,try_catch.Exception()))));
	  } else {
		SEND(vm->server,enif_make_tuple3(env,
										 enif_make_atom(env,"result"),
										 enif_make_copy(env, script_ref),
										 enif_make_tuple2(env, enif_make_atom(env,"ok"),
														  js_to_term(res->ctx,env,value))));
	  }
	}
  }
  enif_free_env(ref_env);
  free(buf);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
