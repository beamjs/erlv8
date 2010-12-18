#include "v8.h"

#include "erl_nif.h"

#include <iostream>
#include <cstring>
#include <cmath>

using namespace std;
using namespace __gnu_cxx;

static v8::Persistent<v8::ObjectTemplate> global_template;

static ErlNifResourceType * script_resource;
static ErlNifResourceType * fun_resource;
static ErlNifResourceType * obj_resource;

class Script; //fwd

typedef struct _script_res_t { 
  Script * script;
} script_res_t;

typedef struct _fun_res_t { 
  v8::Persistent<v8::Context> ctx;
  v8::Persistent<v8::Function> fun;
  Script * script;
} fun_res_t;

typedef struct _obj_res_t { 
  v8::Persistent<v8::Context> ctx;
  v8::Persistent<v8::Object> obj;
} obj_res_t;

static ErlNifEnv * fun_holder_env;

v8::Handle<v8::Value> term_to_js(ErlNifEnv *env, ERL_NIF_TERM term); // fwd
ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val); // fwd

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

class Send {
public:
  ErlNifPid * pid;
  ErlNifEnv * env;

  Send(ErlNifPid *a_pid) : pid(a_pid) {
	env = enif_alloc_env();
	
  };
  
  ~Send() {
	enif_free_env(env);
  };

  void send(ERL_NIF_TERM term) {
	enif_send(NULL, pid, env, term);
	enif_clear_env(env);
  };

};

#define SEND(pid, code) \
  { \
	Send send = Send(pid); \
	ErlNifEnv * env = send.env; \
	send.send(code); \
  } 


class Script {
public:
  v8::Persistent<v8::Context> context;

  ErlNifPid *server;
  ErlNifEnv *env;

  int ticked;
  ERL_NIF_TERM tick;
  ERL_NIF_TERM tick_ref;
  ErlNifCond *tick_cond;
  ErlNifMutex *tick_cond_mtx;

  ErlNifTid tid;
  
  script_res_t * resource;
  

  Script() {
	ticked = 0;
	env = enif_alloc_env();
	tick_cond = enif_cond_create((char *)"erlv8_tick_condition");
	tick_cond_mtx = enif_mutex_create((char *)"erlv8_tick_condition_mutex");
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  context = v8::Context::New(NULL, global_template);
	  v8::Context::Scope context_scope(context);
	  context->Global()->SetHiddenValue(v8::String::New("__erlv8__"),v8::External::New(this));
	}
  };

  ~Script() { 
	context.Dispose();
	enif_free_env(env);
	enif_cond_destroy(tick_cond);
	enif_mutex_unlock(tick_cond_mtx);
	enif_mutex_destroy(tick_cond_mtx);
  };

  void requestTick() {
	SEND(server,enif_make_atom(env,"tick_me"));
  }

  void waitForTick() {
	while (!ticked) { // according to erl_nif/driver documentation, enif_cond_wait might return before the cond was broadcasted
	  enif_cond_wait(tick_cond,tick_cond_mtx);
	}
	ticked = 0;
  }

  void run() {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	}
	ticker(0,0);
  };

  v8::Handle<v8::Value> ticker(ERL_NIF_TERM ref,const v8::Arguments *arguments) {
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
		
		if (!strcmp(name,"stop")) { 
		  free(name);
		  return v8::Undefined(); // just some dummy value
		} else if (((unsigned long) ref) &&
				   (!strcmp(name,"result")) &&
				   (enif_is_identical(array[1],ref))) { // this is our result
		  free(name);
		  
		  v8::Handle<v8::Value> ret = term_to_js(env,array[2]);
		  return ret;
		} else if (!strcmp(name,"call")) { // it is a call
		  ErlNifEnv *msg_env = enif_alloc_env();
		  ERL_NIF_TERM call_ref = enif_make_copy(msg_env, tick_ref);
		  fun_res_t *fun_res;
		  if (enif_get_resource(env,array[1],fun_resource,(void **)(&fun_res))) {
			ERL_NIF_TERM head, tail;
			ERL_NIF_TERM current = array[2];
			unsigned int alen;
			
			enif_get_list_length(env,array[2],&alen);
			
			v8::Local<v8::Value> *args = NULL;
			args = new v8::Local<v8::Value>[alen];
			int i = 0;
			while (enif_get_list_cell(env, current, &head, &tail)) {
			  args[i] = v8::Local<v8::Value>::New(term_to_js(env,head));
			  i++; current = tail;
			}
			v8::Local<v8::Value> call_result = fun_res->fun->Call(fun_res->ctx->Global(), alen, args);
			SEND(server,
				 enif_make_tuple3(env,
								  enif_make_atom(env,"result"),
								  enif_make_copy(env,call_ref),
								  js_to_term(env,call_result)));

			enif_free_env(msg_env);
			delete [] args;
			args = NULL;
		  }
		} else if (!strcmp(name,"script")) { 
		  ErlNifEnv *msg_env = enif_alloc_env();
		  ERL_NIF_TERM script_ref = enif_make_copy(msg_env, tick_ref);

		  unsigned len;
		  enif_get_list_length(env, array[1], &len);
		  char * buf = (char *) malloc(len + 1);
		  enif_get_string(env,array[1],buf,len + 1, ERL_NIF_LATIN1);

		  v8::TryCatch try_catch;

		  v8::Handle<v8::String> script = v8::String::New(buf, len);
		  v8::Handle<v8::Script> compiled = v8::Script::Compile(script);


		  if (compiled.IsEmpty()) {
			SEND(server,
				 (enif_make_tuple3(env,
								   enif_make_atom(env,"compilation_failed"),
								   enif_make_copy(env, script_ref),
								   js_to_term(env,try_catch.Exception()))));
		  } else {
			SEND(server, enif_make_tuple2(env,
										  enif_make_atom(env,"starting"),
										  enif_make_copy(env, script_ref)));
			v8::Handle<v8::Value> value = compiled->Run();
			if (value.IsEmpty()) {
			  SEND(server,enif_make_tuple3(env,
										   enif_make_atom(env,"exception"),
										   enif_make_copy(env, script_ref),
										   js_to_term(env,try_catch.Exception())));
			} else {
			  SEND(server,enif_make_tuple3(env,
										   enif_make_atom(env,"finished"),
										   enif_make_copy(env, script_ref),
										   js_to_term(env,value)));
			}
		  }
		  enif_free_env(msg_env);
		  free(buf);
		} else if ((unsigned long) ref) { // retick if we don't need this tick
		  SEND(server,
			   enif_make_tuple2(env,
								enif_make_atom(env,"retick"),
								enif_make_copy(env,tick_ref)));
		}
		
		free(name);
	  }
	}

  }
};


void * start_script(void *data) {
  Script *script = reinterpret_cast<Script *>(data);
  script->run();
  delete script;
  return NULL;
};


static ERL_NIF_TERM new_script(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM term;

  Script *script = new Script();

  script_res_t *ptr = (script_res_t *)enif_alloc_resource(script_resource, sizeof(script_res_t));
  ptr->script = script;
  script->resource = ptr;
  
  term = enif_make_resource(env, ptr);

  enif_release_resource(ptr);
 
  return term;
};

static ERL_NIF_TERM set_server(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) 
{
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	res->script->server = (ErlNifPid *) malloc(sizeof(ErlNifPid));
	enif_get_local_pid(env, argv[1], res->script->server);
	enif_thread_create((char *)"erlv8", &res->script->tid, start_script, res->script, NULL);
	return enif_make_atom(env,"ok");
  };
  return enif_make_badarg(env);
};

static ERL_NIF_TERM to_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->script->context);
	  
	  return js_to_term(env,term_to_js(env,argv[1])->ToString());
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM to_detail_string(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->script->context);
	  
	  return js_to_term(env,term_to_js(env,argv[1])->ToDetailString());
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM tick(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	{
	  if ((!enif_is_ref(env, argv[1])))
		return enif_make_badarg(env);
	  res->script->tick = enif_make_copy(res->script->env, argv[2]);
	  res->script->tick_ref = enif_make_copy(res->script->env, argv[1]);
	  res->script->ticked = 1;
	  enif_cond_broadcast(res->script->tick_cond);
	  return enif_make_atom(env,"tack");
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM global(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->script->context);
	  v8::Handle<v8::Object> global = res->script->context->Global();
	  return js_to_term(env,global);
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM to_proplist(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  obj_res_t *res;
  if (enif_get_resource(env,argv[0],obj_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->ctx);
	  
	  v8::Handle<v8::Array> keys = res->obj->GetPropertyNames();
	  
	  ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * keys->Length());

	  for (unsigned int i=0;i<keys->Length();i++) {
		v8::Handle<v8::Value> key = keys->Get(v8::Integer::New(i));
		arr[i] = enif_make_tuple2(env,
								  js_to_term(env,v8::Handle<v8::String>::Cast(key)),
								  js_to_term(env,res->obj->Get(key)));
	  }
	  ERL_NIF_TERM list = enif_make_list_from_array(env,arr,keys->Length());
	  free(arr);
	  return list;
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_set(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  obj_res_t *res;
  if (enif_get_resource(env,argv[0],obj_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->ctx);
	  res->obj->Set(term_to_js(env,argv[1]),term_to_js(env,argv[2]));
	  return argv[2];
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_get(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  obj_res_t *res;
  if (enif_get_resource(env,argv[0],obj_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->ctx);
	  return js_to_term(env, res->obj->Get(term_to_js(env,argv[1])));
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_set_proto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  obj_res_t *res;
  if ((enif_get_resource(env,argv[0],obj_resource,(void **)(&res)))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->ctx);
	  return enif_make_atom(env, res->obj->SetPrototype(term_to_js(env,argv[1])) ? "true" : "false");
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ERL_NIF_TERM object_get_proto(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  obj_res_t *res;
  if (enif_get_resource(env,argv[0],obj_resource,(void **)(&res))) {
	{
	  v8::Locker locker;
	  v8::HandleScope handle_scope;
	  v8::Context::Scope context_scope(res->ctx);
	  return js_to_term(env, res->obj->GetPrototype());
	}
  } else {
	return enif_make_badarg(env);
  };
};

static ErlNifFunc nif_funcs[] =
{
  {"new_script", 0, new_script},
  {"set_server", 2, set_server},
  {"global",1, global},
  {"to_string",2, to_string},
  {"to_detail_string",2, to_detail_string},
  {"to_proplist",1, to_proplist},
  {"tick",3, tick},
  {"object_set",3, object_set},
  {"object_get",2, object_get},
  {"object_set_proto",2, object_set_proto},
  {"object_get_proto",1, object_get_proto}
};

#define __ERLV8__(O) v8::Local<v8::External>::Cast(O->GetHiddenValue(v8::String::New("__erlv8__")))->Value()


v8::Handle<v8::Value> WrapFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  Script * script = (Script *)__ERLV8__(v8::Context::GetCurrent()->Global());

  ERL_NIF_TERM term = enif_make_copy(script->env,(ERL_NIF_TERM) arguments.Data()->ToInteger()->Value());

  script_res_t *ptr = (script_res_t *)enif_alloc_resource(script_resource, sizeof(script_res_t));
  ptr->script = script;

  v8::Local<v8::Array> array = v8::Array::New(arguments.Length());

  for (signed int i=0;i<arguments.Length();i++) {
      array->Set(i,arguments[i]);
  }
  // each call gets a unique ref
  ERL_NIF_TERM ref = enif_make_ref(script->env);
  // send invocation request
  SEND(script->server,
	   enif_make_tuple3(env,
						enif_make_copy(env,term),
						enif_make_tuple6(env, 
										 enif_make_atom(env,"erlv8_fun_invocation"),
										 enif_make_atom(env,arguments.IsConstructCall() ? "true" : "false"),
										 js_to_term(env, arguments.Holder()),
										 js_to_term(env, arguments.This()),
										 enif_make_copy(env, ref),
										 enif_make_pid(env, script->server)),
						js_to_term(env,array)));
  return script->ticker(ref, &arguments);
};


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
	} else if (strcmp(name,"undefined")==0) {
	  result = v8::Undefined();
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
  } 
  else if (enif_is_empty_list(env,term)) {
	return v8::Local<v8::Object>::New(v8::Object::New());
  } else if (enif_is_list(env,term)) {
	  // try it as a string
	  unsigned len;
	  enif_get_list_length(env, term, &len);
	  char * str = (char *) malloc(len + 1);
	  if (enif_get_string(env, term, str, len + 1, ERL_NIF_LATIN1)) {
		v8::Handle<v8::String> s = v8::String::New((const char *)str);
		free(str);
		return s;
	  }
	  // if it is not a string, it is a list
	  free(str);
	  ERL_NIF_TERM head, tail;
	  ERL_NIF_TERM current = term;
	  v8::Handle<v8::Object> arr = v8::Array::New(len);
	  int i = 0;
	  while (enif_get_list_cell(env, current, &head, &tail)) {
		arr->Set(v8::Integer::New(i),term_to_js(env,head));
		i++; current = tail;
	  }
	  return v8::Local<v8::Object>::New(arr);
  } else if (enif_is_tuple(env, term)) {
	ERL_NIF_TERM *array;
	int arity;
	enif_get_tuple(env,term,&arity,(const ERL_NIF_TERM **)&array);
	if (arity == 4) { 	// check if it is a v8_fun
	  unsigned len;
	  enif_get_atom_length(env, array[0], &len, ERL_NIF_LATIN1);
	  char * name = (char *) malloc(len + 1);
	  enif_get_atom(env,array[0],name,len + 1, ERL_NIF_LATIN1);
	  fun_res_t *res;
	  int isv8fun = strcmp(name,"erlv8_fun")==0;
	  free(name);
	  if ((isv8fun) &&
		  (enif_get_resource(env,array[1],fun_resource,(void **)(&res)))){
		return res->fun;
	  } else if ((isv8fun) && (enif_is_fun(env, array[1]))) {
		v8::Handle<v8::Function> f = v8::Handle<v8::Function>::Cast(term_to_js(env,array[1]));
		v8::Handle<v8::Object> o = v8::Handle<v8::Object>::Cast(term_to_js(env,array[3]));
		
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
	  // check if it is an object
	  int isobj = strcmp(name,"erlv8_object")==0;
	  free(name);
	  if (isobj) {
		obj_res_t *res;
		if (enif_get_resource(env,array[1],obj_resource,(void **)(&res))) {
		  return res->obj;
		} else if (enif_is_proplist(env,array[1])) {
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
		}

	  }
	  if (iserror) {
		return v8::Exception::Error(v8::Handle<v8::String>::Cast(term_to_js(env,array[1])));
	  }
	  if (isthrow) {
		return v8::ThrowException(term_to_js(env, array[1]));
	  }
	}

  } else if (enif_is_fun(env, term)) {
	ERL_NIF_TERM term2 = enif_make_copy(fun_holder_env,term);

    v8::Local<v8::FunctionTemplate> t = v8::FunctionTemplate::New(WrapFun,v8::Integer::NewFromUnsigned(term2));
	v8::Local<v8::Function> f = v8::Local<v8::Function>::Cast(t->GetFunction());
	return f;
  }
  return v8::Undefined(); // if nothing else works, return undefined
};


ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val) {
  v8::HandleScope handle_scope;
  if (val->IsFunction()) {  // the reason why this check is so high up here is because it is also an object, so it should be before any object.
	fun_res_t *ptr = (fun_res_t *)enif_alloc_resource(fun_resource, sizeof(fun_res_t));
	Script * script = (Script *) v8::External::Unwrap(v8::Context::GetCurrent()->Global()->GetHiddenValue(v8::String::New("__erlv8__")));

	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
	ptr->fun = v8::Persistent<v8::Function>::New(v8::Handle<v8::Function>::Cast(val));
	ptr->script = script;

	obj_res_t *ptr1 = (obj_res_t *)enif_alloc_resource(obj_resource, sizeof(obj_res_t));
	ptr1->obj = v8::Persistent<v8::Object>::New(v8::Handle<v8::Object>::Cast(val));
	ptr1->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());

	ERL_NIF_TERM term = enif_make_tuple4(env,enif_make_atom(env,"erlv8_fun"), 
										 enif_make_resource(env, ptr),
										 enif_make_pid(env, script->server),
										 enif_make_tuple2(env,
														  enif_make_atom(env,"erlv8_object"),
														  enif_make_resource(env, ptr1)));
	enif_release_resource(ptr);
	enif_release_resource(ptr1);

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
    v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(val);
	ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
	for (unsigned int i=0;i<array->Length();i++) {
        arr[i] = js_to_term(env,array->Get(v8::Integer::NewFromUnsigned(i)));
	}
	ERL_NIF_TERM list = enif_make_list_from_array(env,arr,array->Length());
	free(arr);
	return list;
  } else if (val->IsObject()) {
	obj_res_t *ptr = (obj_res_t *)enif_alloc_resource(obj_resource, sizeof(obj_res_t));
	ptr->obj = v8::Persistent<v8::Object>::New(v8::Handle<v8::Object>::Cast(val));
	ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());

	ERL_NIF_TERM term = enif_make_tuple2(env,
										 enif_make_atom(env, "erlv8_object"),
										 enif_make_resource(env, ptr));
	enif_release_resource(ptr);
	return term;
  } else if (val->IsExternal()) { // passing terms
	ERL_NIF_TERM *term_ref = (ERL_NIF_TERM *) v8::External::Unwrap(val);
	ERL_NIF_TERM term = *term_ref;
	free(term_ref);
	return term;
  } else {
	return enif_make_atom(env,"$unknown");
  }
};


static void script_resource_destroy(ErlNifEnv* env, void* obj) {
};
static void fun_resource_destroy(ErlNifEnv* env, void* obj) {
  fun_res_t * res = reinterpret_cast<fun_res_t *>(obj);
  res->ctx.Dispose();
  res->fun.Dispose();
};
static void obj_resource_destroy(ErlNifEnv* env, void* obj) {
  obj_res_t * res = reinterpret_cast<obj_res_t *>(obj);
  res->obj.Dispose();
};

int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
  script_resource = enif_open_resource_type(env, NULL, "erlv8_script_resource", script_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  fun_resource = enif_open_resource_type(env, NULL, "erlv8_fun_resource", fun_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  obj_resource = enif_open_resource_type(env, NULL, "erlv8_obj_resource", obj_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);

  fun_holder_env = enif_alloc_env();

  v8::V8::Initialize();
  v8::HandleScope handle_scope;

  global_template = v8::Persistent<v8::ObjectTemplate>::New(v8::ObjectTemplate::New());

  v8::Locker::StartPreemption(100);

  return 0;
};

void unload(ErlNifEnv *env, void* priv_data)
{
  v8::Locker::StopPreemption();
  global_template.Dispose();
  enif_free_env(fun_holder_env);
};

ERL_NIF_INIT(erlv8_nif,nif_funcs,load,NULL,NULL,unload)
