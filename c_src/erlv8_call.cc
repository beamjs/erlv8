#include "erlv8.hh"

void ErlangFun(VM * vm, ERL_NIF_TERM term, ERL_NIF_TERM ref, v8::Handle<v8::Object> recv, v8::Handle<v8::Array> array); // fwd

TickHandler(CallTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM call_ref = enif_make_copy(ref_env, tick_ref);
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
	  args[i] = v8::Local<v8::Value>::New(term_to_js(fun_res->ctx, vm->isolate, vm->env,head));
	  i++; current = tail;
	}
	v8::Local<v8::Object> recv;
	if (arity == 4) { // this is specified
	  recv = term_to_js(fun_res->ctx, vm->isolate, vm->env, array[3])->ToObject();
	} else {
	  recv = fun_res->ctx->Global();
	}
	v8::Persistent<v8::Function> f = v8::Persistent<v8::Function>::Cast(fun_res->val);

	if (!*f->GetHiddenValue(vm->string__erlv8__)) { // js function
	  v8::TryCatch try_catch;
	  v8::Local<v8::Value> call_result = f->Call(recv, alen, args);
	  if (call_result.IsEmpty()) {
	    SEND(vm->server,
		 enif_make_tuple3(env,
				  enif_make_atom(env,"result"),
				  enif_make_copy(env,call_ref),
				  enif_make_tuple2(env,
						   enif_make_atom(env,"throw"),
						   enif_make_tuple2(env,
								    enif_make_atom(env,"error"),
								    js_to_term(fun_res->ctx, 
									       vm->isolate, 
									       env, 
									       try_catch.Exception())))));
	  } else {
	    SEND(vm->server,
		 enif_make_tuple3(env,
				  enif_make_atom(env,"result"),
				  enif_make_copy(env,call_ref),
				  js_to_term(fun_res->ctx,
					     vm->isolate,
					     env,
					     call_result)));
	  }
	} else { // native Erlang function
	  v8::Local<v8::Array> array = v8::Array::New(alen);

	  for (unsigned int i=0;i<alen;i++) {
		array->Set(i,args[i]);
	  }

	  ErlangFun(vm, external_to_term(f->GetHiddenValue((vm->string__erlv8__))), call_ref, recv, array);
	}

	delete [] args;
	args = NULL;
  }
  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

void ErlangFun(VM * vm, ERL_NIF_TERM term, ERL_NIF_TERM ref, v8::Handle<v8::Object> recv, v8::Handle<v8::Array> array) {
  v8::HandleScope handle_scope;
 
  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
  for (unsigned int i=0;i<array->Length();i++) {
	arr[i] = js_to_term(vm->context,
			    vm->isolate,
			    vm->env,
			    array->Get(v8::Integer::NewFromUnsigned(i)));
  }
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,array->Length());
  free(arr);
  // send invocation request
  SEND(vm->server,
       enif_make_tuple3(env,
			enif_make_copy(env,term),
			enif_make_tuple7(env, 
					 enif_make_atom(env,"erlv8_fun_invocation"),
					 enif_make_atom(env, "false"),
					 js_to_term(vm->context, vm->isolate, env, recv), // FIXME: not quite sure it's right
					 js_to_term(vm->context, vm->isolate, env, recv),
					 enif_make_copy(env, ref),
					 enif_make_pid(env, vm->server),
					 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
};

