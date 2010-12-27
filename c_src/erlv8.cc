#include "erlv8.hh"

typedef TickHandlerResolution (*TickHandler)(VM *, char *, ERL_NIF_TERM, int, const ERL_NIF_TERM*, v8::Handle<v8::Value>&);

struct ErlV8TickHandler {
  const char * name;
  TickHandler handler;
};

static ErlV8TickHandler tick_handlers[] =
{
  {"stop", StopTickHandler},
  {"result", ResultTickHandler},
  {"call", CallTickHandler},
  {"inst", InstantiateTickHandler},
  {"get", GetTickHandler},
  {"set", SetTickHandler},
  {"proplist", ProplistTickHandler},
  {"list", ListTickHandler},
  {"script", ScriptTickHandler},
  {NULL, UnknownTickHandler} 
};


VM::VM() {
  ticked = 0;
  env = enif_alloc_env();
  tick_cond = enif_cond_create((char *)"erlv8_tick_condition");
  tick_cond_mtx = enif_mutex_create((char *)"erlv8_tick_condition_mutex");
  v8::Locker locker;
  v8::HandleScope handle_scope;
  context = v8::Context::New(NULL, global_template);
  v8::Context::Scope context_scope(context);
  context->Global()->SetHiddenValue(v8::String::New("__erlv8__"),v8::External::New(this));
};

VM::~VM() { 
	context.Dispose();
	enif_free_env(env);
	enif_cond_destroy(tick_cond);
#ifdef __APPLE__
	enif_mutex_unlock(tick_cond_mtx);
#endif
	enif_mutex_destroy(tick_cond_mtx);
};

void VM::requestTick() {
  SEND(server,enif_make_atom(env,"tick_me"));
};

void VM::waitForTick() {
  enif_mutex_lock(tick_cond_mtx);
  while (!ticked) { // according to erl_nif/driver documentation, enif_cond_wait might return before the cond was broadcasted
	enif_cond_wait(tick_cond,tick_cond_mtx);
  }
  enif_mutex_unlock(tick_cond_mtx);
  ticked = 0;
};

void VM::run() {
  ticker(0);
};

v8::Handle<v8::Value> VM::ticker(ERL_NIF_TERM ref) {
  v8::Locker locker;
  v8::Context::Scope context_scope(context);
  
  while (1) {
	v8::Unlocker unlocker;
	requestTick();
	waitForTick(); 
	v8::Locker locker;
	v8::HandleScope handle_scope;
	
	if (enif_is_tuple(env, tick)) { // should be always true, just a sanity check
	  
	  ERL_NIF_TERM *array;
	  int arity;
	  enif_get_tuple(env,tick,&arity,(const ERL_NIF_TERM **)&array);
		
	  unsigned len;
	  enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
	  char * name = (char *) malloc(len + 1);
	  enif_get_atom(env,array[0],name,len + 1, ERL_NIF_LATIN1);
	  
	  // lookup the matrix
	  v8::Handle<v8::Value> result;
	  unsigned int i = 0;
	  bool stop_flag = false;

	  while (!stop_flag) {
		if ((!tick_handlers[i].name) ||
			(!strcmp(name,tick_handlers[i].name))) { // handler has been located
		  switch (tick_handlers[i].handler(this, name, ref, arity, array, result)) {
		  case DONE:
			stop_flag = true;
			break;
		  case NEXT:
			break;
		  case RETURN:
			return result;
			break;
		  }
		}
		i++;
	  }
	  free(name);
	}
  }
};


void * start_vm(void *data) {
  VM *vm = reinterpret_cast<VM *>(data);
  vm->run();
  delete vm;
  return NULL;
};


static ERL_NIF_TERM new_vm(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM term;

  VM *vm = new VM();

  vm_res_t *ptr = (vm_res_t *)enif_alloc_resource(vm_resource, sizeof(vm_res_t));
  ptr->vm = vm;
  vm->resource = ptr;
  
  term = enif_make_resource(env, ptr);

  enif_release_resource(ptr);
 
  return term;
};

static ERL_NIF_TERM set_server(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) 
{
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	res->vm->server = (ErlNifPid *) malloc(sizeof(ErlNifPid));
	enif_get_local_pid(env, argv[1], res->vm->server);
	enif_thread_create((char *)"erlv8", &res->vm->tid, start_vm, res->vm, NULL);
	return enif_make_atom(env,"ok");
  };
  return enif_make_badarg(env);
};

static ERL_NIF_TERM context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) 
{
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	LHCS(res->vm->context);

	ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
	
	ERL_NIF_TERM term = enif_make_resource(env, ptr);

	enif_release_resource(ptr);
	
	return term;
  };
  return enif_make_badarg(env);
};

static ERL_NIF_TERM to_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->vm->context);
	  
	  return js_to_term(env,term_to_js(env,argv[1])->ToString());
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM to_detail_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	{
	  LHCS(res->vm->context);
	  return js_to_term(env,term_to_js(env,argv[1])->ToDetailString());
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM tick(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	if ((!enif_is_ref(env, argv[1])))
	  return enif_make_badarg(env);
	res->vm->tick = enif_make_copy(res->vm->env, argv[2]);
	res->vm->tick_ref = enif_make_copy(res->vm->env, argv[1]);
	res->vm->ticked = 1;
	enif_cond_broadcast(res->vm->tick_cond);
	return enif_make_atom(env,"tack");
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM global(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  ctx_res_t *res;
  if (enif_get_resource(env,argv[0],ctx_resource,(void **)(&res))) {
	LHCS(res->ctx);
	v8::Handle<v8::Object> global = res->ctx->Global();
	return js_to_term(env,global);
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM new_context(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	LHCS(res->vm->context);
	v8::Persistent<v8::Context> context = v8::Context::New(NULL, global_template);
	context->Global()->SetHiddenValue(v8::String::New("__erlv8__"),v8::External::New(res->vm));

	ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
	ptr->ctx = context;
	
	ERL_NIF_TERM term = enif_make_resource(env, ptr);
	
	enif_release_resource(ptr);
	
	return term;
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM value_taint(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	LHCS(res->vm->context);
	return js_to_term(env,term_to_js(env,argv[1]));
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_set_hidden(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if (enif_get_resource(env,argv[0],val_resource,(void **)(&res))) {
	LHCS(res->ctx);
	res->val->ToObject()->SetHiddenValue(term_to_js(env,argv[1])->ToString(),term_to_js(env,argv[2]));
	return argv[2];
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_get_hidden(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if (enif_get_resource(env,argv[0],val_resource,(void **)(&res))) {
	LHCS(res->ctx);
	return js_to_term(env, res->val->ToObject()->GetHiddenValue(term_to_js(env,argv[1])->ToString()));
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_set_proto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if ((enif_get_resource(env,argv[0],val_resource,(void **)(&res)))) {
	LHCS(res->ctx);
	return enif_make_atom(env, res->val->ToObject()->SetPrototype(term_to_js(env,argv[1])) ? "true" : "false");
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_get_proto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if (enif_get_resource(env,argv[0],val_resource,(void **)(&res))) {
	LHCS(res->ctx);
	return js_to_term(env, res->val->ToObject()->GetPrototype());
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_delete(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if (enif_get_resource(env,argv[0],val_resource,(void **)(&res))) {
	LHCS(res->ctx);
	v8::Handle<v8::Value> key = term_to_js(env,argv[1]);
	if (key->IsString()) {
	  res->val->ToObject()->Delete(key->ToString());
	} else if (key->IsNumber()) {
	  res->val->ToObject()->Delete(key->Uint32Value());
	}
	return enif_make_atom(env,"ok");
  } else {
	return enif_make_badarg(env);
  };
};

v8::Handle<v8::Value> GetterFun(v8::Local<v8::String> property,const v8::AccessorInfo &info); // fwd
void SetterFun(v8::Local<v8::String> property,v8::Local<v8::Value> value,const v8::AccessorInfo &info); // fwd

void weak_accessor_data_cleaner(v8::Persistent<v8::Value> object, void * data) {
  if (object.IsNearDeath()) {
	object->ToObject()->DeleteHiddenValue(v8::String::New("_getter"));
	object->ToObject()->DeleteHiddenValue(v8::String::New("_setter"));
  }
}

static ERL_NIF_TERM object_set_accessor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  if (enif_get_resource(env,argv[0],val_resource,(void **)(&res))) {
	LHCS(res->ctx);
	if (argc > 2) {
	  v8::Handle<v8::Value> name = term_to_js(env,argv[1]);
	  if (!name->IsString()) 
		return enif_make_badarg(env);

	  v8::AccessorGetter getter = GetterFun;
	  v8::AccessorSetter setter = 0;
	  v8::Persistent<v8::Object> data = v8::Persistent<v8::Object>::New(v8::Object::New());
	  data.MakeWeak(NULL,weak_accessor_data_cleaner); // so that we'll release externals when we're done

	  if (term_to_js(env,argv[2])->IsUndefined()) {
		return enif_make_badarg(env);
	  } else {
		data->SetHiddenValue(v8::String::New("_getter"), term_to_external(argv[2]));
	  }

	  if (argc > 3) {
		setter = SetterFun;
		data->SetHiddenValue(v8::String::New("_setter"), term_to_external(argv[3]));
	  }

	  v8::AccessControl access_control = v8::DEFAULT;
   
	  if (argc > 4 && enif_is_atom(env, argv[4])) {
		unsigned len;
		enif_get_atom_length(env, argv[4], &len, ERL_NIF_LATIN1);
		char * name = (char *) malloc(len + 1);
		enif_get_atom(env,argv[4],name,len + 1, ERL_NIF_LATIN1);
		if (!strcmp(name,"default")) {
		  access_control = v8::DEFAULT;
		} else if (!strcmp(name,"all_can_read")) {
		  access_control = v8::ALL_CAN_READ;
		} else if (!strcmp(name,"all_can_write")) {
		  access_control = v8::ALL_CAN_WRITE;
		} else if (!strcmp(name,"prohibits_overwriting")) {
		  access_control = v8::PROHIBITS_OVERWRITING;
		}
		free(name);
	  }

	  v8::PropertyAttribute property_attribute = v8::None;
   
	  if (argc > 5) {
		property_attribute = term_to_property_attribute(env,argv[5]);
	  }

	  return enif_make_atom(env, res->val->ToObject()->SetAccessor(name->ToString(), getter, setter, data,
																   access_control, property_attribute) ? "true" : "false");
	} else {
	  return enif_make_badarg(env);
	}
  } else {
	return enif_make_badarg(env);
  };
};


static ERL_NIF_TERM value_equals(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res1; 
  val_res_t *res2; 
  if ((enif_get_resource(env,argv[1],val_resource,(void **)(&res1))) &&
	  (enif_get_resource(env,argv[2],val_resource,(void **)(&res2)))) {
	LHCS(res1->ctx);
	return enif_make_atom(env, res1->val->ToObject()->Equals(res2->val->ToObject()) ? "true" : "false");
  } else {
	vm_res_t *res;
	if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	  LHCS(res->vm->context);
	  return enif_make_atom(env, term_to_js(env,argv[1])->Equals(term_to_js(env,argv[2])) ? "true" : "false");
	}
  };
  return enif_make_badarg(env);
};

static ERL_NIF_TERM value_strict_equals(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res1; 
  val_res_t *res2; 
  if ((enif_get_resource(env,argv[1],val_resource,(void **)(&res1))) &&
	  (enif_get_resource(env,argv[2],val_resource,(void **)(&res2)))) {
	LHCS(res1->ctx);
	return enif_make_atom(env, res1->val->ToObject()->StrictEquals(res2->val->ToObject()) ? "true" : "false");
  } else {
	vm_res_t *res;
	if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	  LHCS(res->vm->context);
	  return enif_make_atom(env, term_to_js(env,argv[1])->StrictEquals(term_to_js(env,argv[2])) ? "true" : "false");
	}
  };
  return enif_make_badarg(env);
};

static ErlNifFunc nif_funcs[] =
{
  {"new_vm", 0, new_vm},
  {"set_server", 2, set_server},
  {"context", 1, context},
  {"new_context", 1, new_context},
  {"global",1, global},
  {"to_string",2, to_string},
  {"to_detail_string",2, to_detail_string},
  {"tick",3, tick},
  {"object_set_hidden",3, object_set_hidden},
  {"object_get_hidden",2, object_get_hidden},
  {"object_set_proto",2, object_set_proto},
  {"object_get_proto",1, object_get_proto},
  {"object_delete",2, object_delete},
  {"object_set_accessor", 3, object_set_accessor},
  {"object_set_accessor", 4, object_set_accessor},
  {"object_set_accessor", 5, object_set_accessor},
  {"object_set_accessor", 6, object_set_accessor},
  {"object_set_accessor", 7, object_set_accessor},
  {"value_equals",3, value_equals},
  {"value_strict_equals",3, value_strict_equals},
  {"value_taint",2, value_taint}
};

#define __ERLV8__(O) v8::Local<v8::External>::Cast(O->GetHiddenValue(v8::String::New("__erlv8__")))->Value()


v8::Handle<v8::Value> EmptyFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  return v8::Undefined();
}

v8::Handle<v8::Value> WrapFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  ERL_NIF_TERM term = enif_make_copy(vm->env,external_to_term(arguments.Data()));
 
  ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
  ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());

  v8::Local<v8::Array> array = v8::Array::New(arguments.Length());

  for (signed int i=0;i<arguments.Length();i++) {
      array->Set(i,arguments[i]);
  }
  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);
  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
  for (unsigned int i=0;i<array->Length();i++) {
	arr[i] = js_to_term(vm->env,array->Get(v8::Integer::NewFromUnsigned(i)));
  }
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,array->Length());
  free(arr);
  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,term),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,arguments.IsConstructCall() ? "true" : "false"),
										 js_to_term(env, arguments.Holder()),
										 js_to_term(env, arguments.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_resource(env, ptr)
										 ),
						enif_make_copy(env,arglist)));
  enif_release_resource(ptr);
  return vm->ticker(ref);
};

v8::Handle<v8::Value> GetterFun(v8::Local<v8::String> property,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();
  
  ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
  ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());

  ERL_NIF_TERM term = enif_make_copy(vm->env,external_to_term(data->GetHiddenValue(v8::String::New("_getter"))));

  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);

  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * 1);
  arr[0] = js_to_term(vm->env, property);
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,1);
  free(arr);

  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,term),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(env, info.Holder()),
										 js_to_term(env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_resource(env, ptr)
										 ),
						enif_make_copy(env,arglist)));
  enif_release_resource(ptr);
  return vm->ticker(ref);  
}

void SetterFun(v8::Local<v8::String> property,v8::Local<v8::Value> value,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();
  ERL_NIF_TERM term = enif_make_copy(vm->env,external_to_term(data->GetHiddenValue(v8::String::New("_setter"))));

  ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
  ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());

  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);

  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * 2);
  arr[0] = js_to_term(vm->env, property);
  arr[1] = js_to_term(vm->env, value);
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,2);
  free(arr);

  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,term),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(env, info.Holder()),
										 js_to_term(env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_resource(env, ptr)
										 ),
						enif_make_copy(env,arglist)));
  enif_release_resource(ptr);
  vm->ticker(ref);  
}



static void vm_resource_destroy(ErlNifEnv* env, void* obj) {
};

static void val_resource_destroy(ErlNifEnv* env, void* obj) {
  val_res_t * res = reinterpret_cast<val_res_t *>(obj);
  res->ctx.Dispose();
  res->val.Dispose();
};

static void ctx_resource_destroy(ErlNifEnv* env, void* obj) {
  ctx_res_t * res = reinterpret_cast<ctx_res_t *>(obj);
  res->ctx.Dispose();
};


int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
  vm_resource = enif_open_resource_type(env, NULL, "erlv8_vm_resource", vm_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  val_resource = enif_open_resource_type(env, NULL, "erlv8_val_resource", val_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  ctx_resource = enif_open_resource_type(env, NULL, "erlv8_ctx_resource", ctx_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  v8::V8::Initialize();
  v8::HandleScope handle_scope;

  global_template = v8::Persistent<v8::ObjectTemplate>::New(v8::ObjectTemplate::New());
  
  int preemption = 100; // default value
  enif_get_int(env, load_info, &preemption);
  v8::Locker::StartPreemption(preemption);

  return 0;
};

void unload(ErlNifEnv *env, void* priv_data)
{
  v8::Locker::StopPreemption();
  global_template.Dispose();
};

v8::Persistent<v8::ObjectTemplate> global_template;
ErlNifResourceType * ctx_resource;
ErlNifResourceType * vm_resource;
ErlNifResourceType * val_resource;

ERL_NIF_INIT(erlv8_nif,nif_funcs,load,NULL,NULL,unload)
