#include "erlv8.hh"

void ErlangConstFun(VM * vm, ERL_NIF_TERM term, ERL_NIF_TERM ref, v8::Handle<v8::Object> instance, v8::Handle<v8::Array> array); // fwd

TickHandler(InstantiateTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM inst_ref = enif_make_copy(ref_env, tick_ref);
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

    v8::Handle<v8::Function> f = v8::Handle<v8::Function>::Cast(fun_res->val);

    if (!*f->GetHiddenValue(vm->string__erlv8__)) { // js function
      v8::TryCatch try_catch;
      v8::Local<v8::Value> inst_result = f->NewInstance(alen, args);
      if (inst_result.IsEmpty()) {
        SEND(vm->server,
             enif_make_tuple3(env,
                              enif_make_atom(env,"result"),
                              enif_make_copy(env,inst_ref),
                              enif_make_tuple2(env,
                                               enif_make_atom(env,"throw"),
                                               enif_make_tuple2(env,
                                                                enif_make_atom(env,"error"),
                                                                js_to_term(fun_res->ctx,vm->isolate, env,try_catch.Exception())))));
      } else {
        SEND(vm->server,
             enif_make_tuple3(env,
                              enif_make_atom(env,"result"),
                              enif_make_copy(env,inst_ref),
                              js_to_term(fun_res->ctx,vm->isolate, env,inst_result)));
      }
    } else { // native Erlang function
      v8::Local<v8::Array> array = v8::Array::New(alen);

      for (unsigned int i=0;i<alen;i++) {
        array->Set(i,args[i]);
      }

      ErlangConstFun(vm,
                     external_to_term(f->GetHiddenValue(v8::String::New("__erlv8__"))),
                     inst_ref,
                     vm->empty_constructor->GetFunction()->NewInstance(),
                     array);
    }

    delete [] args;
    args = NULL;
  }
  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

void ErlangConstFun(VM * vm, ERL_NIF_TERM term, ERL_NIF_TERM ref, v8::Handle<v8::Object> instance, v8::Handle<v8::Array> array) {
  v8::HandleScope handle_scope;

  ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
  ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
  for (unsigned int i=0;i<array->Length();i++) {
    arr[i] = js_to_term(vm->context,vm->isolate, vm->env,array->Get(v8::Integer::NewFromUnsigned(i)));
  }
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,array->Length());
  free(arr);
  // send invocation request
  SEND(vm->server,
       enif_make_tuple3(env,
                        enif_make_copy(env,term),
                        enif_make_tuple7(env,
                                         enif_make_atom(env,"erlv8_fun_invocation"),
                                         enif_make_atom(env, "true"),
                                         js_to_term(vm->context, vm->isolate, env, instance), // FIXME: not quite sure it's right
                                         js_to_term(vm->context, vm->isolate, env, instance),
                                         enif_make_copy(env, ref),
                                         enif_make_pid(env, vm->server),
                                         enif_make_resource(env, ptr)
                                         ),
                        enif_make_copy(env,arglist)));
  enif_release_resource(ptr);
};
