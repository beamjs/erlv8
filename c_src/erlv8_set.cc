#include "erlv8.hh"

TickHandler(SetTickHandler) {
  ErlNifEnv *tmp_env = enif_alloc_env();
  ERL_NIF_TERM value = enif_make_copy(tmp_env, array[3]); // stashing it away since Set() might call an accessor, which might change vm->env
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	v8::PropertyAttribute property_attribute = v8::None;

	if (arity == 5) {
	  property_attribute = term_to_property_attribute(vm->env,array[4]);
	}
	
	obj_res->val->ToObject()->Set(term_to_js(obj_res->ctx, vm->env,array[2]),term_to_js(obj_res->ctx, tmp_env,value), property_attribute);
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,tick_ref),
						  enif_make_copy(env,value)));
  } 
  enif_free_env(tmp_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(SetProtoTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

    const char *atom_val = obj_res->val->ToObject()->SetPrototype(term_to_js(obj_res->ctx,vm->env,array[2])) ? "true" : "false";
	
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,tick_ref),
						  enif_make_atom(env,atom_val)));
  } 
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(SetHiddenTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	obj_res->val->ToObject()->SetHiddenValue(term_to_js(obj_res->ctx,vm->env,array[2])->ToString(),term_to_js(obj_res->ctx,vm->env,array[3]));

	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,tick_ref),
						  enif_make_copy(env,array[2])));
  } 
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(SetInternalTickHandler) {
  val_res_t *obj_res;
  char name[MAX_ATOM_LEN];
  unsigned len;

  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	int index;
	enif_get_int(vm->env, array[2], &index);

	if (index < 0 || (index + 1 > obj_res->val->ToObject()->InternalFieldCount())) {
	  		SEND(vm->server,
			 enif_make_tuple3(env,
							  enif_make_atom(env,"result"),
							  enif_make_copy(env,tick_ref),
							  enif_make_atom(env,"error")));
	} else {

	  if (!strcmp(tick_name,"set_internal_extern")) {
		enif_get_atom_length(vm->env, array[4], &len, ERL_NIF_LATIN1);
		enif_get_atom(vm->env,array[4],(char *)&name,len + 1, ERL_NIF_LATIN1);
		v8::Handle<v8::Object> proto = extern_name_to_proto(vm, name);
		obj_res->val->ToObject()->SetInternalField(index,term_to_external(array[3]));
	  } else {
		obj_res->val->ToObject()->SetInternalField(index,term_to_js(obj_res->ctx,vm->env,array[3]));
	  }
	  
	  SEND(vm->server,
		   enif_make_tuple3(env,
							enif_make_atom(env,"result"),
							enif_make_copy(env,tick_ref),
						  enif_make_copy(env,array[3])));
	} 
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

v8::Handle<v8::Value> GetterFun(v8::Local<v8::String> property,const v8::AccessorInfo &info); // fwd
void SetterFun(v8::Local<v8::String> property,v8::Local<v8::Value> value,const v8::AccessorInfo &info); // fwd

void weak_accessor_data_cleaner(v8::Persistent<v8::Value> object, void * data) {
  if (object.IsNearDeath()) {
	object->ToObject()->DeleteHiddenValue(v8::String::New("_getter"));
	object->ToObject()->DeleteHiddenValue(v8::String::New("_setter"));
        object.Dispose();
        object.Clear();
  }
}

TickHandler(SetAccessorTickHandler) {
  char aname[MAX_ATOM_LEN];
  const char *atom_val;
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
	LHCS(obj_res->ctx);

	if (arity > 3) {
	  v8::Handle<v8::Value> name = term_to_js(obj_res->ctx,vm->env,array[2]);
	  if (!name->IsString()) {
          goto badarg;
      }
	  v8::AccessorGetter getter = GetterFun;
	  v8::AccessorSetter setter = 0;
	  v8::Persistent<v8::Object> data = v8::Persistent<v8::Object>::New(v8::Object::New());
	  data.MakeWeak(NULL,weak_accessor_data_cleaner); // so that we'll release externals when we're done

	  if (term_to_js(obj_res->ctx,vm->env,array[3])->IsUndefined()) {
          goto badarg;
	  } else {
		data->SetHiddenValue(v8::String::New("_getter"), term_to_external(array[3]));
	  }

	  if (arity > 4) {
		setter = SetterFun;
		data->SetHiddenValue(v8::String::New("_setter"), term_to_external(array[4]));
	  }

	  v8::AccessControl access_control = v8::DEFAULT;
   
	  if (arity > 5 && enif_is_atom(vm->env, array[5])) {
		unsigned len;
		enif_get_atom_length(vm->env, array[5], &len, ERL_NIF_LATIN1);
		enif_get_atom(vm->env,array[5], (char *) &aname,len + 1, ERL_NIF_LATIN1);
		if (!strcmp(aname,"default")) {
		  access_control = v8::DEFAULT;
		} else if (!strcmp(aname,"all_can_read")) {
		  access_control = v8::ALL_CAN_READ;
		} else if (!strcmp(aname,"all_can_write")) {
		  access_control = v8::ALL_CAN_WRITE;
		} else if (!strcmp(aname,"prohibits_overwriting")) {
		  access_control = v8::PROHIBITS_OVERWRITING;
		}
	  }

	  v8::PropertyAttribute property_attribute = v8::None;
   
	  if (arity > 6) {
		property_attribute = term_to_property_attribute(vm->env,array[6]);
	  }

      atom_val = obj_res->val->ToObject()->SetAccessor(name->ToString(), getter, setter, data,
																   access_control, property_attribute) ? "true" : "false";
      goto send;
	}
badarg:
    atom_val = "badarg";
send:
	SEND(vm->server,
		 enif_make_tuple3(env,
						  enif_make_atom(env,"result"),
						  enif_make_copy(env,tick_ref),
						  enif_make_atom(env, atom_val)));
  } 
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}



v8::Handle<v8::Value> GetterFun(v8::Local<v8::String> property,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();
  
  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);

  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * 1);
  arr[0] = js_to_term(vm->context, vm->env, property);
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,1);
  free(arr);

  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,external_to_term(data->GetHiddenValue(v8::String::New("_getter")))),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(vm->context, env, info.Holder()),
										 js_to_term(vm->context, env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
  return handle_scope.Close(vm->ticker(ref));  
}

void SetterFun(v8::Local<v8::String> property,v8::Local<v8::Value> value,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();

  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);

  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * 2);
  arr[0] = js_to_term(vm->context, vm->env, property);
  arr[1] = js_to_term(vm->context, vm->env, value);
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,2);
  free(arr);

  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,external_to_term(data->GetHiddenValue(v8::String::New("_setter")))),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(vm->context, env, info.Holder()),
										 js_to_term(vm->context, env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
  vm->ticker(ref);  
}
