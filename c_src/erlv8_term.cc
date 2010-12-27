#include "erlv8.hh"

int enif_is_proplist(ErlNifEnv * env, ERL_NIF_TERM term)
{
  ERL_NIF_TERM head, tail;
  ERL_NIF_TERM current = term;
  int arity;
  ERL_NIF_TERM *array;
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
	  return 0;
	}

	current = tail;
  }
  return 1;
}

v8::PropertyAttribute term_to_property_attribute(ErlNifEnv * env, ERL_NIF_TERM term) {
  if (enif_is_atom(env, term)) {
	  v8::PropertyAttribute property_attribute;
	  unsigned len;
	  enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1);
	  char * name = (char *) malloc(len + 1);
	  enif_get_atom(env,term,name,len + 1, ERL_NIF_LATIN1);
	  if (!strcmp(name,"none")) {
		property_attribute = v8::None;
	  } else if (!strcmp(name,"readonly")) {
		property_attribute = v8::ReadOnly;
	  } else if (!strcmp(name,"dontenum")) {
		property_attribute = v8::DontEnum;
	  } else if (!strcmp(name,"dontdelete")) {
		property_attribute = v8::DontDelete;
	  }
	  free(name);
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
  }
}

inline v8::Handle<v8::Value> term_to_external(ERL_NIF_TERM term) {
  term_ref_t * term_ref = (term_ref_t *) enif_alloc(sizeof(term_ref_t));	
  term_ref->env = enif_alloc_env();										
  term_ref->term = enif_make_copy(term_ref->env, term);
  v8::Persistent<v8::External> obj = v8::Persistent<v8::External>::New(v8::External::New(term_ref));
  obj.MakeWeak(NULL,weak_external_cleaner);
  return obj;
}

inline ERL_NIF_TERM external_to_term(v8::Handle<v8::Value> val) {
	term_ref_t * term_ref = (term_ref_t *) v8::External::Unwrap(v8::Handle<v8::External>::Cast(val));
	return term_ref->term;
}

v8::Handle<v8::Value> term_to_js(ErlNifEnv *env, ERL_NIF_TERM term) {
  int _int; unsigned int _uint; long _long; unsigned long _ulong; ErlNifSInt64 _int64; ErlNifUInt64 _uint64; double _double;
  if (enif_is_atom(env, term)) {
	unsigned len;
	enif_get_atom_length(env, term, &len, ERL_NIF_LATIN1);
	char * name = (char *) malloc(len + 1);
	enif_get_atom(env,term,name,len + 1, ERL_NIF_LATIN1);
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
	  result = v8::Local<v8::String>::New(v8::String::New(name));
	}
	free(name);
	return result;
  } else if	(enif_get_int(env,term,&_int)) {
	return v8::Local<v8::Integer>::New(v8::Integer::New(_int));
  } else if (enif_get_uint(env,term,&_uint)) {
	return v8::Local<v8::Integer>::New(v8::Integer::NewFromUnsigned(_uint));
  } else if (enif_get_long(env,term,&_long)) {
	return v8::Local<v8::Number>::New(v8::Number::New(_long));
  } else if (enif_get_ulong(env,term,&_ulong)) {
	return v8::Local<v8::Number>::New(v8::Number::New(_ulong));
  } else if (enif_get_int64(env,term,&_int64)) {
	return v8::Local<v8::Number>::New(v8::Number::New(_int64));
  } else if (enif_get_uint64(env,term,&_uint64)) {
	return v8::Local<v8::Number>::New(v8::Number::New(_uint64));
  } else if (enif_get_double(env,term,&_double)) {
	return v8::Local<v8::Number>::New(v8::Number::New(_double));
  } else if (enif_is_list(env,term)) { // string
	unsigned len;
	enif_get_list_length(env, term, &len);
	char * str = (char *) malloc(len + 1);
	if (enif_get_string(env, term, str, len + 1, ERL_NIF_LATIN1)) {
	  v8::Handle<v8::String> s = v8::String::New((const char *)str);
	  free(str);
	  return s;
	  }
	free(str);
  } else if (enif_is_tuple(env, term)) {
	ERL_NIF_TERM *array;
	int arity;
	enif_get_tuple(env,term,&arity,(const ERL_NIF_TERM **)&array);
	if (arity == 3) { 
	  unsigned len;
	  enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
	  char * name = (char *) malloc(len + 1);
	  enif_get_atom(env,array[0],name,len + 1, ERL_NIF_LATIN1);
	  val_res_t *res;
	  // check if it is a v8_fun
	  int isv8fun = strcmp(name,"erlv8_fun")==0;
	  // check if it is an object
	  int isobj = strcmp(name,"erlv8_object")==0;
	  // check if it is an array
	  int isarray = strcmp(name,"erlv8_array")==0;
	  free(name);

	  if (isobj||isarray) {
		val_res_t *res;
		if (enif_get_resource(env,array[1],val_resource,(void **)(&res))) {
		  return res->val->ToObject();
		} else if (isobj && enif_is_proplist(env,array[1])) {
		  v8::Handle<v8::Object> obj = v8::Object::New();
		  ERL_NIF_TERM head, tail;
		  ERL_NIF_TERM current = array[1];
		  int arity;
		  ERL_NIF_TERM *arr;
		  while (enif_get_list_cell(env, current, &head, &tail)) {
			enif_get_tuple(env,head,&arity,(const ERL_NIF_TERM **)&arr);
			obj->Set(term_to_js(env,arr[0]),
					 term_to_js(env,arr[1]));

			current = tail;
		  }
		  return v8::Local<v8::Object>::New(obj);
		} else if (isarray && enif_is_list(env, array[1])) {
		  unsigned int i,alen;
		  ERL_NIF_TERM head, tail;
		  ERL_NIF_TERM current = array[1];

		  enif_get_list_length(env, current, &alen);

		  v8::Handle<v8::Array> arrobj = v8::Array::New(alen);

		  i = 0;
		  while (enif_get_list_cell(env, current, &head, &tail)) {
			arrobj->Set(v8::Integer::New(i), term_to_js(env,head));
			current = tail;
			i++;
		  }
		  return v8::Local<v8::Array>::New(arrobj);
		}
	  }

	  if ((isv8fun) &&
		  (enif_get_resource(env,array[1],val_resource,(void **)(&res)))){
		return res->val;
	  } else if ((isv8fun) && (enif_is_fun(env, array[1]))) {
		v8::Handle<v8::Function> f = v8::Handle<v8::Function>::Cast(term_to_js(env,array[1]));
		v8::Handle<v8::Object> o = v8::Handle<v8::Object>::Cast(term_to_js(env,array[2]));
		
		v8::Handle<v8::Array> keys = o->GetPropertyNames();

		for (unsigned int i=0;i<keys->Length();i++) {
		  v8::Handle<v8::Value> key = keys->Get(v8::Integer::New(i));
		  f->Set(key,o->Get(key));
		}
	  
	    return f;

	  }
	  
	}

	if (arity == 2) { 
	  unsigned len;
	  enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
	  char * name = (char *) malloc(len + 1);
	  enif_get_atom(env,array[0],name,len + 1, ERL_NIF_LATIN1);
	  // check if it is an error
	  int iserror = strcmp(name,"error")==0;
	  int isthrow = strcmp(name,"throw")==0;
	  if (iserror) {
		return v8::Exception::Error(v8::Handle<v8::String>::Cast(term_to_js(env,array[1])));
	  }
	  if (isthrow) {
		return v8::ThrowException(term_to_js(env, array[1]));
	  }
	}

  } else if (enif_is_fun(env, term)) {
    v8::Local<v8::FunctionTemplate> t = v8::FunctionTemplate::New(WrapFun,term_to_external(term));
    v8::Local<v8::FunctionTemplate> empty_t = v8::FunctionTemplate::New(EmptyFun);

	v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(t->GetFunction());
	f->SetHiddenValue(v8::String::New("__erlv8__"), term_to_external(term));
	f->SetHiddenValue(v8::String::New("__erlv8__empty__constructor__"), empty_t->GetFunction());

	return f;
  } else if ((enif_is_pid(env, term)) || (enif_is_ref(env,term))) {
	return term_to_external(term);
  }
  return v8::Undefined(); // if nothing else works, return undefined
};


ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val) {
  v8::HandleScope handle_scope;
  if (val.IsEmpty()) {
	return enif_make_atom(env,"undefined");
  } else if (val->IsFunction()) {  // the reason why this check is so high up here is because it is also an object, so it should be before any object.
	val_res_t *ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
	VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));

	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
	ptr->val = v8::Persistent<v8::Function>::New(v8::Handle<v8::Function>::Cast(val));

	ERL_NIF_TERM term = enif_make_tuple3(env,enif_make_atom(env,"erlv8_fun"), 
										 enif_make_resource(env, ptr),
										 enif_make_pid(env, vm->server));

	return term;
  } else if	(val->IsUndefined()) {
    return enif_make_atom(env,"undefined");
  } else if (val->IsNull()) {
	return enif_make_atom(env,"null");
  } else if (val->IsTrue()) {
	return enif_make_atom(env,"true");
  } else if (val->IsFalse()) {
	return enif_make_atom(env,"false");
  } else if (val->IsString()) {
    return enif_make_string(env,*v8::String::AsciiValue(val->ToString()),ERL_NIF_LATIN1);
  } else if (val->IsInt32()) {
	return enif_make_long(env,val->ToInt32()->Value());
  } else if (val->IsUint32()) {
	return enif_make_int64(env,val->ToUint32()->Value());
  } else if (val->IsNumber()) {
	double d = val->ToNumber()->Value();
	if (d == round(d)) {
	  return enif_make_int64(env,d);
	} else {
	  return enif_make_double(env,d);
	}
  } else if (val->IsArray()) {
	val_res_t *ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
	ptr->val = v8::Persistent<v8::Array>::New(v8::Handle<v8::Array>::Cast(val));
	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
	VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));

	ERL_NIF_TERM term = enif_make_tuple3(env,
										 enif_make_atom(env, "erlv8_array"),
										 enif_make_resource(env, ptr),
										 enif_make_pid(env, vm->server)
										 );

	return term;
  } else if (val->IsObject()) {
	val_res_t *ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
	ptr->val = v8::Persistent<v8::Object>::New(v8::Handle<v8::Object>::Cast(val));
	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
	VM * vm = (VM *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));

	ERL_NIF_TERM term = enif_make_tuple3(env,
										 enif_make_atom(env, "erlv8_object"),
										 enif_make_resource(env, ptr),
										 enif_make_pid(env, vm->server)
										 );

	return term;
  } else if (val->IsExternal()) { // passing terms
	return enif_make_copy(env,external_to_term(val));
  } else {
	return enif_make_atom(env,"$unknown");
  }
};
