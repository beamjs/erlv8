#include "erlv8.hh"

typedef TickHandlerResolution (*TickHandler)(VM *, char *, ERL_NIF_TERM, ERL_NIF_TERM, ERL_NIF_TERM, int, const ERL_NIF_TERM*, v8::Handle<v8::Value>&);

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
  {"delete", DeleteTickHandler},
  {"taint", TaintTickHandler},
  {"equals", EqualsTickHandler},
  {"strict_equals", StrictEqualsTickHandler},
  {"get", GetTickHandler},
  {"get_proto", GetProtoTickHandler},
  {"get_hidden", GetHiddenTickHandler},
  {"set", SetTickHandler},
  {"set_proto", SetProtoTickHandler},
  {"set_hidden", SetHiddenTickHandler},
  {"proplist", ProplistTickHandler},
  {"list", ListTickHandler},
  {"script", ScriptTickHandler},
  {"gc", GCTickHandler},
  {"to_string", ToStringTickHandler},
  {"to_detail_string", ToDetailStringTickHandler},
  {"extern_proto", ExternProtoTickHandler},
  {"externalize", ExternalizeTickHandler},
  {"internal_count", InternalCountTickHandler},
  {"set_internal", SetInternalTickHandler},
  {"set_internal_extern", SetInternalTickHandler},
  {"get_internal", GetInternalTickHandler},
  {NULL, UnknownTickHandler} 
};


VM::VM() {
  env = enif_alloc_env();
  v8::Locker locker;
  v8::HandleScope handle_scope;
  context = v8::Context::New(NULL, global_template);
  v8::Context::Scope context_scope(context);
  tid = enif_thread_self();

  context->Global()->SetHiddenValue(string__erlv8__,v8::External::New(this));

  ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
  ptr->ctx = v8::Persistent<v8::Context>::New(context);
  ERL_NIF_TERM resource_term = enif_make_resource(env, ptr);
  enif_release_resource(ptr);

  context->Global()->SetHiddenValue(v8::String::New("__erlv8__ctx__"),term_to_external(resource_term));

  external_proto_num = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_atom = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_bin = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_ref = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_fun = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_port = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_pid = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_tuple = v8::Persistent<v8::Object>::New(external_template->NewInstance());
  external_proto_list = v8::Persistent<v8::Object>::New(external_template->NewInstance());

  push_socket = zmq_socket(zmq_context, ZMQ_PUSH);
  ticker_push_socket = zmq_socket(zmq_context, ZMQ_PUSH);
  pull_socket = zmq_socket(zmq_context, ZMQ_PULL);

  char socket_id[64];
  sprintf(socket_id, "inproc://tick-publisher-%ld", (long int) this);

  char ticker_socket_id[64];
  sprintf(ticker_socket_id, "inproc://tick-publisher-ticker-%ld", (long int) this);

  zmq_bind(push_socket, socket_id);
  zmq_bind(ticker_push_socket, ticker_socket_id);
  zmq_connect(pull_socket, socket_id);
  zmq_connect(pull_socket, ticker_socket_id);

};

VM::~VM() { 
	context.Dispose();
	external_proto_num.Dispose();
	external_proto_atom.Dispose();
	external_proto_bin.Dispose();
	external_proto_ref.Dispose();
	external_proto_fun.Dispose();
	external_proto_port.Dispose();
	external_proto_pid.Dispose();
	external_proto_tuple.Dispose();
	external_proto_list.Dispose();

	enif_free_env(env);

	zmq_close(push_socket);
	zmq_close(ticker_push_socket);
	zmq_close(pull_socket);
};

void VM::run() {
  ticker(0);
};

v8::Handle<v8::Value> VM::ticker(ERL_NIF_TERM ref0) {
  v8::Locker locker;
  v8::Context::Scope context_scope(context);

  char name[MAX_ATOM_LEN];
  unsigned len;

  ErlNifEnv * ref_env = enif_alloc_env();
  ERL_NIF_TERM ref;

  if ((unsigned long) ref0 == 0) {
	ref = ref0;
	DEBUG(server, enif_make_atom(env, "current_ticker"), enif_make_atom(env, "top"));
  } else {
	ref = enif_make_copy(ref_env, ref0);
	DEBUG(server, enif_make_atom(env, "current_ticker"), enif_make_copy(env, ref));
  }


  zmq_msg_t msg;
  Tick tick_s;
  ERL_NIF_TERM tick, tick_ref;
 
  while (1) {
	v8::HandleScope handle_scope;

	{
	  v8::Unlocker unlocker;
	  zmq_msg_init (&msg);
	  zmq_recv (pull_socket, &msg, 0);
	  memcpy(&tick_s, zmq_msg_data(&msg), sizeof(Tick));
	  tick = enif_make_copy(env, tick_s.tick);
	  tick_ref = enif_make_copy(env, tick_s.ref);
	  enif_free_env(tick_s.env);
	  zmq_msg_close(&msg);
	}

	DEBUG(server, enif_make_tuple2(env, enif_make_atom(env, "last_tick"), (unsigned long) ref == 0 ? enif_make_atom(env,"top") :
	                                    enif_make_copy(env, ref)),	enif_make_copy(env, tick));
	
	if (enif_is_tuple(env, tick)) { // should be always true, just a sanity check
	  
	  ERL_NIF_TERM *array;
	  int arity;
	  enif_get_tuple(env,tick,&arity,(const ERL_NIF_TERM **)&array);
		
	  enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
	  enif_get_atom(env,array[0],(char *)&name,len + 1, ERL_NIF_LATIN1);
	  
	  // lookup the matrix
	  v8::Handle<v8::Value> result;
	  unsigned int i = 0;
	  bool stop_flag = false;

	  while (!stop_flag) {
		if ((!tick_handlers[i].name) ||
			(!strcmp(name,tick_handlers[i].name))) { // handler has been located
		  switch (tick_handlers[i].handler(this, name, tick, tick_ref, ref, arity, array, result)) {
		  case DONE:
			stop_flag = true;
			break;
		  case NEXT:
			break;
		  case RETURN:
			enif_free_env(ref_env);
			enif_clear_env(env);
			zmq_msg_t tick_msg;
			int e;

			while (!pop_ticks.empty()) {
			  Tick newtick = pop_ticks.front();
			  pop_ticks.pop();
			  zmq_msg_init_size(&tick_msg, sizeof(Tick));
              memcpy(zmq_msg_data(&tick_msg), &newtick, sizeof(Tick));

			  do {
				e = zmq_send(ticker_push_socket, &tick_msg, ZMQ_NOBLOCK);
			  } while (e == EAGAIN);

			  zmq_msg_close(&tick_msg);
			}

			return result;
			break;
		  }
		}
		i++;
	  }
	}
	enif_clear_env(env);
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

static ERL_NIF_TERM tick(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  vm_res_t *res;
  int e;
  if (enif_get_resource(env,argv[0],vm_resource,(void **)(&res))) {
	if ((!enif_is_ref(env, argv[1])))
	  return enif_make_badarg(env);

	zmq_msg_t tick_msg;

	Tick tick;
	tick.env = enif_alloc_env();
	tick.tick = enif_make_copy(tick.env, argv[2]);
	tick.ref = enif_make_copy(tick.env, argv[1]);


    zmq_msg_init_size(&tick_msg, sizeof(Tick));

    memcpy(zmq_msg_data(&tick_msg), &tick, sizeof(Tick));

	do {
	  e = zmq_send(res->vm->push_socket, &tick_msg, ZMQ_NOBLOCK);
	} while (e == EAGAIN);

	zmq_msg_close(&tick_msg);

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
	context->Global()->SetHiddenValue(string__erlv8__,v8::External::New(res->vm));

	ctx_res_t *ptr = (ctx_res_t *)enif_alloc_resource(ctx_resource, sizeof(ctx_res_t));
	ptr->ctx = v8::Persistent<v8::Context>::New(context);
	
	ERL_NIF_TERM resource_term = enif_make_resource(env, ptr);
	
	enif_release_resource(ptr);
	
	context->Global()->SetHiddenValue(v8::String::New("__erlv8__ctx__"),term_to_external(resource_term));

	return resource_term;
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
        object.Dispose();
        object.Clear();
  }
}

static ERL_NIF_TERM object_set_accessor(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  val_res_t *res;
  char aname[MAX_ATOM_LEN];
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
		enif_get_atom(env,argv[4], (char *) &aname,len + 1, ERL_NIF_LATIN1);
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



static ErlNifFunc nif_funcs[] =
{
  {"new_vm", 0, new_vm},
  {"set_server", 2, set_server},
  {"context", 1, context},
  {"new_context", 1, new_context},
  {"global",1, global},
  {"tick",3, tick},
  {"object_set_accessor", 3, object_set_accessor},
  {"object_set_accessor", 4, object_set_accessor},
  {"object_set_accessor", 5, object_set_accessor},
  {"object_set_accessor", 6, object_set_accessor},
  {"object_set_accessor", 7, object_set_accessor}
};

#define __ERLV8__(O) v8::Local<v8::External>::Cast(O->GetHiddenValue(string__erlv8__))->Value()


v8::Handle<v8::Value> EmptyFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  return v8::Undefined();
}

v8::Handle<v8::Value> WrapFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  v8::Locker locker;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(vm->env);
  // prepare arguments
  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * arguments.Length());
  for (int i=0;i<arguments.Length();i++) {
	arr[i] = js_to_term(vm->env,arguments[i]);
  }
  ERL_NIF_TERM arglist = enif_make_list_from_array(vm->env,arr,arguments.Length());
  free(arr);
  // send invocation request
  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,external_to_term(arguments.Data())),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,arguments.IsConstructCall() ? "true" : "false"),
										 js_to_term(env, arguments.Holder()),
										 js_to_term(env, arguments.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
  return vm->ticker(ref);
};

v8::Handle<v8::Value> GetterFun(v8::Local<v8::String> property,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();
  
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
						enif_make_copy(env,external_to_term(data->GetHiddenValue(v8::String::New("_getter")))),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(env, info.Holder()),
										 js_to_term(env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
  return vm->ticker(ref);  
}

void SetterFun(v8::Local<v8::String> property,v8::Local<v8::Value> value,const v8::AccessorInfo &info) {
  v8::HandleScope handle_scope;
  VM * vm = (VM *)__ERLV8__(v8::Context::GetCurrent()->Global());

  v8::Local<v8::Object> data = info.Data()->ToObject();

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
						enif_make_copy(env,external_to_term(data->GetHiddenValue(v8::String::New("_setter")))),
						enif_make_tuple7(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,"false"),
										 js_to_term(env, info.Holder()),
										 js_to_term(env, info.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, vm->server),
										 enif_make_copy(env, external_to_term(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__ctx__"))))
										 ),
						enif_make_copy(env,arglist)));
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

  zmq_context = zmq_init(0); // we are using inproc only, so no I/O threads

  vm_resource = enif_open_resource_type(env, NULL, "erlv8_vm_resource", vm_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  val_resource = enif_open_resource_type(env, NULL, "erlv8_val_resource", val_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  ctx_resource = enif_open_resource_type(env, NULL, "erlv8_ctx_resource", ctx_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  v8::V8::Initialize();
  v8::HandleScope handle_scope;

  global_template = v8::Persistent<v8::ObjectTemplate>::New(v8::ObjectTemplate::New());

  external_template = v8::Persistent<v8::ObjectTemplate>::New(v8::ObjectTemplate::New());

  empty_constructor = v8::Persistent<v8::FunctionTemplate>::New(v8::FunctionTemplate::New(EmptyFun));

  string__erlv8__ = v8::Persistent<v8::String>::New(v8::String::New("__erlv8__"));

  int preemption = 100; // default value
  enif_get_int(env, load_info, &preemption);
  v8::Locker::StartPreemption(preemption);

  return 0;
};

void unload(ErlNifEnv *env, void* priv_data)
{
  v8::Locker::StopPreemption();
  global_template.Dispose();
  external_template.Dispose();
  empty_constructor.Dispose();
  string__erlv8__.Dispose();
  zmq_term(zmq_context);
};

int reload(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  return 0;
}

int upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info) {
  return 0;
}


v8::Persistent<v8::ObjectTemplate> global_template;
v8::Persistent<v8::ObjectTemplate> external_template;
v8::Persistent<v8::FunctionTemplate> empty_constructor;
v8::Persistent<v8::String> string__erlv8__;

ErlNifResourceType * ctx_resource;
ErlNifResourceType * vm_resource;
ErlNifResourceType * val_resource;

void *zmq_context;

ERL_NIF_INIT(erlv8_nif,nif_funcs,load,reload,upgrade,unload)
