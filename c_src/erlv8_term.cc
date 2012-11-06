#include "erlv8.hh"

int enif_is_proplist(ErlNifEnv * env, ERL_NIF_TERM term)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM current = term;
  int arity;
  ERL_NIF_TERM *array;
  ErlNifBinary string_binary;

  if (!enif_is_list(env,term)) {
        return 0;
  }
  while (enif_get_list_cell(env, current, &head, &tail)) {
        if (!enif_is_tuple(env,head)) return 0; // not a tuple -> not a proplist
        enif_get_tuple(env,head,&arity,(const ERL_NIF_TERM **)&array);
        if (arity != 2) return 0; // does not consist of two elements -> not a proplist
        if (enif_is_list(env, array[0])) {
          unsigned len;
          enif_get_list_length(env, array[0], &len);
          char * str = (char *) malloc(len + 1);
          if (!enif_get_string(env, array[0], str, len + 1, ERL_NIF_LATIN1)) {
                free(str);
                return 0;
          }
          free(str);
        } else if (!enif_is_atom(env, array[0])) {
      if (enif_inspect_iolist_as_binary(env, array[0], &string_binary)) { // string
        return 1;
      }
          return 0;
        }

        current = tail;
  }
  return 1;
}

v8::PropertyAttribute term_to_property_attribute(ErlNifEnv * env, ERL_NIF_TERM term) {
  unsigned len;
  char name[MAX_ATOM_LEN];

  if (enif_is_atom(env, term)) {
    v8::PropertyAttribute property_attribute;
    enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1);
    enif_get_atom(env,term, (char *) &name,len + 1, ERL_NIF_LATIN1);
    if (!strcmp(name,"none")) {
      property_attribute = v8::None;
    } else if (!strcmp(name,"readonly")) {
      property_attribute = v8::ReadOnly;
    } else if (!strcmp(name,"dontenum")) {
      property_attribute = v8::DontEnum;
    } else if (!strcmp(name,"dontdelete")) {
      property_attribute = v8::DontDelete;
    } else { // always return something
      property_attribute = v8::None;
    }
    return property_attribute;
  } else if (enif_is_list(env, term)) {
        ERL_NIF_TERM current = term;
        ERL_NIF_TERM head, tail;
        v8::PropertyAttribute property_attribute = v8::None;
        while (enif_get_list_cell(env, current, &head, &tail)) {
          property_attribute = (v8::PropertyAttribute) (property_attribute | term_to_property_attribute(env,head));
          current = tail;
        }
        return property_attribute;
  } else {
        return v8::None;
  }
}

void weak_external_cleaner(v8::Persistent<v8::Value> object, void * data) {
  if (object.IsNearDeath()) {
        term_ref_t * term_ref = (term_ref_t *) v8::External::Unwrap(v8::Handle<v8::External>::Cast(object));
        enif_free_env(term_ref->env);
        enif_free(term_ref);
    object.Dispose();
    object.Clear();
        v8::V8::AdjustAmountOfExternalAllocatedMemory(-(long)sizeof(term_ref_t));
  }
}

inline v8::Handle<v8::Value> term_to_external(ERL_NIF_TERM term) {
  v8::HandleScope handle_scope;
  term_ref_t * term_ref = (term_ref_t *) enif_alloc(sizeof(term_ref_t));
  term_ref->env = enif_alloc_env();
  term_ref->term = enif_make_copy(term_ref->env, term);
  v8::Persistent<v8::External> obj = v8::Persistent<v8::External>::New(v8::External::New(term_ref));
  obj.MakeWeak(NULL,weak_external_cleaner);
  v8::V8::AdjustAmountOfExternalAllocatedMemory((long)sizeof(term_ref_t));
  return obj;
}

inline ERL_NIF_TERM external_to_term(v8::Handle<v8::Value> val) {
        term_ref_t * term_ref = (term_ref_t *) v8::External::Unwrap(v8::Handle<v8::External>::Cast(val));
        return term_ref->term;
}

v8::Handle<v8::Object> externalize_term(map<ERL_NIF_TERM, v8::Handle<v8::Object>, cmp_erl_nif_term> cache, v8::Handle<v8::Object> proto, ERL_NIF_TERM term) {
  v8::HandleScope handle_scope;
  map<ERL_NIF_TERM, v8::Handle<v8::Object>, cmp_erl_nif_term>::iterator iter = cache.find(term);

  if (iter != cache.end()) {
    return handle_scope.Close(iter->second); // it was cached before
  } else {
    v8::Handle<v8::Value> external = term_to_external(term);
    v8::Handle<v8::Object> obj = v8::Object::New();
    obj->SetPrototype(proto);
    obj->SetHiddenValue(v8::String::New("__erlv8__"), external);
    cache.insert(std::pair<ERL_NIF_TERM, v8::Handle<v8::Object> >(term, obj)); // cache it
    return handle_scope.Close(obj);
  }

}

v8::Handle<v8::Value> term_to_js(v8::Handle<v8::Context> ctx,  v8::Isolate* isolate, ErlNifEnv *env, ERL_NIF_TERM term) {
  TRACE("(%p) term_to_js - 1\n", isolate);
  LHCS(isolate, ctx);
  TRACE("(%p) term_to_js - 2\n", isolate);
  int _int; unsigned int _uint; long _long; unsigned long _ulong; ErlNifSInt64 _int64; ErlNifUInt64 _uint64; double _double;
  ErlNifBinary string_binary;
  unsigned len;
  char name[MAX_ATOM_LEN];

  if (enif_is_atom(env, term)) {
    TRACE("(%p) term_to_js - 3 a\n", isolate);
    enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1);
    enif_get_atom(env, term, (char *) &name,len + 1, ERL_NIF_LATIN1);
    v8::Handle<v8::Value> result;
    // check for special atoms
    if (strcmp(name,"false")==0) {
      result = v8::Local<v8::Boolean>::New(v8::Boolean::New(0));
    } else if (strcmp(name,"true")==0) {
      result = v8::Local<v8::Boolean>::New(v8::Boolean::New(1));
    } else if (strcmp(name,"ok")==0) {
      result = v8::Local<v8::Boolean>::New(v8::Boolean::New(1));
    } else if (strcmp(name,"undefined")==0) {
      result = v8::Undefined();
    } else if (strcmp(name,"null")==0) {
      result = v8::Null();
    } else { // if it is not a special atom, convert it to a string
      result = v8::String::New(name);
    }
    return handle_scope.Close(result);
  } else if (enif_get_int(env,term,&_int)) {
    TRACE("(%p) term_to_js - 3 b\n", isolate);
    return handle_scope.Close(v8::Local<v8::Integer>::New(v8::Integer::New(_int)));
  } else if (enif_get_uint(env,term,&_uint)) {
    TRACE("(%p) term_to_js - 3 c\n", isolate);
    return handle_scope.Close(v8::Local<v8::Integer>::New(v8::Integer::NewFromUnsigned(_uint)));
  } else if (enif_get_long(env,term,&_long)) {
    TRACE("(%p) term_to_js - 3 d\n", isolate);
    return handle_scope.Close(v8::Local<v8::Number>::New(v8::Number::New(_long)));
  } else if (enif_get_ulong(env,term,&_ulong)) {
    TRACE("(%p) term_to_js - 3 e\n", isolate);
    return handle_scope.Close(v8::Local<v8::Number>::New(v8::Number::New(_ulong)));
  } else if (enif_get_int64(env,term,&_int64)) {
    TRACE("(%p) term_to_js - 3 f\n", isolate);
    return handle_scope.Close(v8::Local<v8::Number>::New(v8::Number::New(_int64)));
  } else if (enif_get_uint64(env,term,&_uint64)) {
    TRACE("(%p) term_to_js - 3 g\n", isolate);
    return handle_scope.Close(v8::Local<v8::Number>::New(v8::Number::New(_uint64)));
  } else if (enif_get_double(env,term,&_double)) {
    TRACE("(%p) term_to_js - 3 h\n", isolate);
    return handle_scope.Close(v8::Local<v8::Number>::New(v8::Number::New(_double)));
  } else if (enif_inspect_iolist_as_binary(env, term, &string_binary)) { // string
    TRACE("(%p) term_to_js - 3 i\n", isolate);
    v8::Local<v8::String> s = v8::String::New((const char *)string_binary.data, string_binary.size);
    if (s->Utf8Length() != string_binary.size)
      printf("%d != %lu\n", s->Utf8Length()-1, string_binary.size);
    return handle_scope.Close(s);
  } else if (enif_is_tuple(env, term)) {
    TRACE("(%p) term_to_js - 3 j\n", isolate);
    ERL_NIF_TERM *array;
    int arity;
    enif_get_tuple(env,term,&arity,(const ERL_NIF_TERM **)&array);
    if (arity == 3) {
      enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
      enif_get_atom(env,array[0], (char *) &name,len + 1, ERL_NIF_LATIN1);
      val_res_t *res;
      // check if it is a v8_fun
      int isv8fun = strcmp(name,"erlv8_fun")==0;
      // check if it is an object
      int isobj = strcmp(name,"erlv8_object")==0;
      // check if it is an array
      int isarray = strcmp(name,"erlv8_array")==0;

      if (isobj||isarray) {
        if (enif_get_resource(env,array[1],val_resource,(void **)(&res))) {
          return handle_scope.Close(res->val->ToObject());
        } else if (isobj && enif_is_proplist(env,array[1])) {
          v8::Local<v8::Object> obj = v8::Object::New();
          ERL_NIF_TERM head, tail;
          ERL_NIF_TERM current = array[1];
          int arity;
          ERL_NIF_TERM *arr;
          while (enif_get_list_cell(env, current, &head, &tail)) {
            enif_get_tuple(env,head,&arity,(const ERL_NIF_TERM **)&arr);
            obj->Set(term_to_js(ctx, isolate, env,arr[0]),
                     term_to_js(ctx, isolate, env,arr[1]));

            current = tail;
          }
          return handle_scope.Close(obj);
        } else if (isarray && enif_is_list(env, array[1])) {
          unsigned int i,alen;
          ERL_NIF_TERM head, tail;
          ERL_NIF_TERM current = array[1];

          enif_get_list_length(env, current, &alen);

          v8::Local<v8::Array> arrobj = v8::Array::New(alen);

          i = 0;
          while (enif_get_list_cell(env, current, &head, &tail)) {
            arrobj->Set(v8::Integer::New(i), term_to_js(ctx, isolate, env,head));
            current = tail;
            i++;
          }
          return handle_scope.Close(arrobj);
        }
      }

      if ((isv8fun) &&
          (enif_get_resource(env,array[1],val_resource,(void **)(&res)))){
        return handle_scope.Close(res->val);
      } else if ((isv8fun) && (enif_is_fun(env, array[1]))) {
        v8::Handle<v8::Function> f = v8::Handle<v8::Function>::Cast(term_to_js(ctx, isolate, env, array[1]));
        v8::Handle<v8::Object> o = v8::Handle<v8::Object>::Cast(term_to_js(ctx, isolate, env, array[2]));

        v8::Local<v8::Array> keys = o->GetPropertyNames();

        for (unsigned int i=0;i<keys->Length();i++) {
          v8::Local<v8::Value> key = keys->Get(v8::Integer::New(i));
          f->Set(key,o->Get(key));
        }

        return handle_scope.Close(f);

      }

    }

    if (arity == 2) {
      enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
      enif_get_atom(env,array[0], (char *) &name,len + 1, ERL_NIF_LATIN1);
      // check if it is an error
      int iserror = strcmp(name,"error")==0;
      int isthrow = strcmp(name,"throw")==0;
      if (iserror) {
        return handle_scope.Close(v8::Exception::Error(v8::Handle<v8::String>::Cast(term_to_js(ctx, isolate, env,array[1]))));
      }
      if (isthrow) {
        return v8::ThrowException(term_to_js(ctx, isolate, env, array[1]));
      }
    }

  } else if (enif_is_fun(env, term)) {
    TRACE("(%p) term_to_js - 3 k\n", isolate);
    VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));
    map<ERL_NIF_TERM, v8::Handle<v8::FunctionTemplate>, cmp_erl_nif_term>::iterator iter = vm->fun_map.find(term);

    if (iter != vm->fun_map.end()) {
      return handle_scope.Close(iter->second->GetFunction()); // it was cached before
    } else {
      v8::Handle<v8::Value> external = term_to_external(term);
      v8::Persistent<v8::FunctionTemplate> t = v8::Persistent<v8::FunctionTemplate>::New(v8::FunctionTemplate::New(WrapFun,external));

      v8::Local<v8::Function> f = t->GetFunction();
      f->SetHiddenValue(vm->string__erlv8__, external);

      vm->fun_map.insert(std::pair<ERL_NIF_TERM, v8::Handle<v8::FunctionTemplate> >(external_to_term(external), t)); // cache it
      return handle_scope.Close(f);
    }
  } else if (enif_is_pid(env, term)) {
    VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));
    return handle_scope.Close(externalize_term(vm->extern_map, vm->external_proto_pid, term));
  } else if (enif_is_ref(env, term)) {
    VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));
    return handle_scope.Close(externalize_term(vm->extern_map, vm->external_proto_ref, term));
  }

  return v8::Undefined(); // if nothing else works, return undefined
};


ERL_NIF_TERM js_to_term(v8::Handle<v8::Context> ctx,  v8::Isolate* isolate, ErlNifEnv *env, v8::Handle<v8::Value> val) {
  TRACE("(%p) js_to_term - 1\n", isolate);
  LHCS(isolate, ctx);
  TRACE("(%p) js_to_term - 2\n", isolate);
  if (val.IsEmpty()) {
    TRACE("(%p) js_to_term - 3 a\n", isolate);
    return enif_make_atom(env,"undefined");
  } else if (val->IsFunction()) {  // the reason why this check is so high up here is because it is also an object, so it should be before any object.
    TRACE("(%p) js_to_term - 3 b\n", isolate);
    val_res_t *ptr;
    TRACE("(%p) js_to_term - 4\n", isolate);
    v8::Handle<v8::Function> fun = v8::Handle<v8::Function>::Cast(val);
    ERL_NIF_TERM resource_term;
    TRACE("(%p) js_to_term - 5\n", isolate);
    ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
    TRACE("(%p) js_to_term - 6\n", isolate);
    ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
    TRACE("(%p) js_to_term - 7\n", isolate);
    ptr->val = v8::Persistent<v8::Function>::New(v8::Handle<v8::Function>::Cast(val));
    TRACE("(%p) js_to_term - 8\n", isolate);
    resource_term = enif_make_resource(env, ptr);
    TRACE("(%p) js_to_term - 9\n", isolate);
    enif_release_resource(ptr);
    TRACE("(%p) js_to_term - 10\n", isolate);
    VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));
    TRACE("(%p) js_to_term - 11\n", isolate);
    ERL_NIF_TERM term = enif_make_tuple3(env,enif_make_atom(env,"erlv8_fun"),
                                         resource_term,
                                         enif_make_pid(env, vm->server));
    TRACE("(%p) js_to_term - 11\n", isolate);

    return term;
  } else if	(val->IsUndefined()) {
    TRACE("(%p) js_to_term - 3 c\n", isolate);
    return enif_make_atom(env,"undefined");
  } else if (val->IsNull()) {
    TRACE("(%p) js_to_term - 3 d\n", isolate);
    return enif_make_atom(env,"null");
  } else if (val->IsTrue()) {
    TRACE("(%p) js_to_term - 3 c\n", isolate);
    return enif_make_atom(env,"true");
  } else if (val->IsFalse()) {
    TRACE("(%p) js_to_term - 3 d\n", isolate);
    return enif_make_atom(env,"false");
  } else if (val->IsString()) {
    TRACE("(%p) js_to_term - 3 e\n", isolate);
    ErlNifBinary result_binary = {0};
    {
      /*      v8::Local<v8::String> S =  val->ToString();
              v8::String::Utf8Value  V = v8::String::Utf8Value(val->ToString());*/
      /*      enif_alloc_binary(S->Utf8Length(), &result_binary);
              S->WriteUtf8((char *) result_binary.data, result_binary.size);*/
    }
    /*    enif_alloc_binary(v8::String::Utf8Value(val->ToString()).length(), &result_binary);
          (void)memcpy(result_binary.data, *v8::String::Utf8Value(val->ToString()), result_binary.size);*/

    enif_alloc_binary(v8::String::Utf8Value(val).length(), &result_binary);
    (void)memcpy(result_binary.data, *v8::String::Utf8Value(val), result_binary.size);


    return enif_make_binary(env, &result_binary);
  } else if (val->IsInt32()) {
    TRACE("(%p) js_to_term - 3 f\n", isolate);
        return enif_make_long(env,val->ToInt32()->Value());
  } else if (val->IsUint32()) {
    TRACE("(%p) js_to_term - 3 g\n", isolate);
        return enif_make_int64(env,val->ToUint32()->Value());
  } else if (val->IsNumber()) {
    TRACE("(%p) js_to_term - 3 h\n", isolate);
    double d = val->ToNumber()->Value();
    if (d == round(d)) {
      return enif_make_int64(env,d);
    } else {
      return enif_make_double(env,d);
    }
  } else if (val->IsArray()) {
    TRACE("(%p) js_to_term - 3 i\n", isolate);
    val_res_t *ptr;
    v8::Handle<v8::Array> arr = v8::Handle<v8::Array>::Cast(val);
    ERL_NIF_TERM resource_term;
    ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
    ptr->val = v8::Persistent<v8::Array>::New(v8::Handle<v8::Array>::Cast(val));
    ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
    resource_term = enif_make_resource(env, ptr);
    enif_release_resource(ptr);

    VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));
    ERL_NIF_TERM term = enif_make_tuple3(env,
                                         enif_make_atom(env, "erlv8_array"),
                                         resource_term,
                                         enif_make_pid(env, vm->server)
                                         );

    return term;
  } else if (val->IsObject()) {
    TRACE("(%p) js_to_term - 3 j\n", isolate);
    v8::Local<v8::Object> obj = val->ToObject();
    TRACE("(%p) js_to_term - 4\n", isolate);
    v8::Local<v8::Context> c = v8::Context::GetCurrent();
    TRACE("(%p) js_to_term - 5\n", isolate);
    v8::Local<v8::Object> g = c->Global();
    TRACE("(%p) js_to_term - 6\n", isolate);
    v8::Local<v8::Value> v = g->GetHiddenValue(v8::String::New("__erlv8__"));
    TRACE("(%p) js_to_term - 7\n", isolate);
    VM * vm = (VM *) v8::External::Unwrap(v);
    TRACE("(%p) js_to_term - 8\n", isolate);
    if (obj->GetPrototype()->Equals(vm->external_proto_num) ||
        obj->GetPrototype()->Equals(vm->external_proto_atom) ||
        obj->GetPrototype()->Equals(vm->external_proto_bin) ||
        obj->GetPrototype()->Equals(vm->external_proto_ref) ||
        obj->GetPrototype()->Equals(vm->external_proto_fun) ||
        obj->GetPrototype()->Equals(vm->external_proto_port) ||
        obj->GetPrototype()->Equals(vm->external_proto_pid) ||
        obj->GetPrototype()->Equals(vm->external_proto_tuple) ||
        obj->GetPrototype()->Equals(vm->external_proto_list)) {
      TRACE("(%p) js_to_term - 7\n", isolate);
      return enif_make_copy(env, external_to_term(v8::Handle<v8::External>::Cast(obj->GetHiddenValue(vm->string__erlv8__))));
    } else {
      TRACE("(%p) js_to_term - 3 k\n", isolate);
      ERL_NIF_TERM resource_term;

      val_res_t *ptr;
      ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
      ptr->val = v8::Persistent<v8::Object>::New(v8::Handle<v8::Object>::Cast(val));
      ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
      resource_term = enif_make_resource(env, ptr);
      enif_release_resource(ptr);

      ERL_NIF_TERM term = enif_make_tuple3(env,
                                           enif_make_atom(env, "erlv8_object"),
                                           resource_term,
                                           enif_make_pid(env, vm->server)
                                           );

      return term;
    }
  } else {
    return enif_make_atom(env,"$unknown");
  }
};
