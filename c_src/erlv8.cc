#include "v8.h"
#include "erl_nif.h"

static v8::Persistent<v8::ObjectTemplate> global_template;
static ErlNifResourceType * script_resource;

class ErlScript {
public:
  ErlNifEnv *caller_env;
  const char *buf;
  unsigned len;

  v8::Persistent<v8::Context> context;

  ErlNifPid *server;
  ErlNifEnv *env;

  ErlScript(ErlNifEnv * a_env, const char *a_buf, unsigned a_len) : caller_env(env), buf(a_buf), len(a_len) {
	env = enif_alloc_env();
  };

  ~ErlScript() { 
	context.Dispose();
	enif_free_env(env);
  };

  void send(ERL_NIF_TERM term) {
	enif_send(NULL, server, env, term);
	enif_clear_env(env);
  };

  void run() {
	v8::HandleScope handle_scope;

	context = v8::Context::New(NULL, global_template);
	v8::Context::Scope context_scope(context);
	context->Global()->SetHiddenValue(v8::String::New("__erlv8__"),v8::External::New(this));

	v8::TryCatch try_catch;
	
	v8::Handle<v8::String> script = v8::String::New(buf, len);
    v8::Handle<v8::Script> compiled = v8::Script::Compile(script);
	
	if (compiled.IsEmpty()) {
	  send(enif_make_atom(env,"compilation_failed"));
	} else {
	  send(enif_make_atom(env,"starting"));
	  v8::Handle<v8::Value> value = compiled->Run();
      send(enif_make_atom(env,"finished"));
	};
};

};

typedef struct _script_res_t { 
  ErlScript * script;
} script_res_t;



static ERL_NIF_TERM new_script(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
  ERL_NIF_TERM term;

  unsigned len;
  enif_get_list_length(env, argv[0], &len);
  char * script = (char *) malloc(len + 1);
  enif_get_string(env,argv[0],script,len + 1, ERL_NIF_LATIN1);

  ErlScript *escript = new ErlScript(env, reinterpret_cast<const char *>(script),len);

  script_res_t *ptr = (script_res_t *)enif_alloc_resource(script_resource, sizeof(script_res_t));
  ptr->script = escript;
  term = enif_make_resource(env, ptr);

  enif_release_resource(ptr);

  return term;
};

static ERL_NIF_TERM get_script(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) 
{
  script_res_t *res;
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
     return enif_make_string(env,res->script->buf, ERL_NIF_LATIN1);
  };
  return enif_make_badarg(env);
};


void * run_script(void *data) {
  ErlScript *escript = reinterpret_cast<ErlScript *>(data);
  escript->run();
  return NULL;
};

static ERL_NIF_TERM run(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[]) {
  script_res_t *res;
  ErlNifTid tid;
  if (!enif_is_pid(env,argv[1])) {
	return enif_make_badarg(env);
  }
  if (enif_get_resource(env,argv[0],script_resource,(void **)(&res))) {
	res->script->server = (ErlNifPid *) malloc(sizeof(ErlNifPid));
	enif_get_local_pid(env, argv[1], res->script->server);
	enif_thread_create((char *)"erlv8", &tid, run_script, res->script, NULL);
  } else {
	return enif_make_badarg(env);
  };
  return enif_make_atom(env,"ok");
};

#include <stdio.h>

static ErlNifFunc nif_funcs[] =
{
  {"new_script", 1, new_script},
  {"get_script", 1, get_script},
  {"run", 2, run}
};

#define __ERLV8__(O) v8::Local<v8::External>::Cast(O->GetHiddenValue(v8::String::New("__erlv8__")))->Value()

ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val) {
  v8::HandleScope handle_scope;
  if (val->IsUndefined()) {
    return enif_make_atom(env,"undefined");
  } else if (val->IsNull()) {
	return enif_make_atom(env,"null");
  } else if (val->IsString()) {
    return enif_make_string(env,*v8::String::AsciiValue(val->ToString()),ERL_NIF_LATIN1);
  } else if (val->IsInt32()) {
	return enif_make_long(env,val->ToInt32()->Value());
  } else if (val->IsArray()) {
    v8::Handle<v8::Array> array = v8::Handle<v8::Array>::Cast(val);
	ERL_NIF_TERM *arr = (ERL_NIF_TERM *) malloc(sizeof(ERL_NIF_TERM) * array->Length());
	for (unsigned int i=0;i<array->Length();i++) {
        arr[0] = js_to_term(env,array->Get(v8::Integer::New(i)));
	}
	ERL_NIF_TERM list = enif_make_list(env,array->Length(),*arr);
	free(arr);
	return list;
  } else if (val->IsObject()) {
  }
  return enif_make_badarg(env);
};

v8::Handle<v8::Value> CallFun(const v8::Arguments &arguments) {
  v8::HandleScope handle_scope;
  ErlScript * script = (ErlScript *)__ERLV8__(v8::Context::GetCurrent()->Global());
  
  ERL_NIF_TERM M = enif_make_atom(script->env,*v8::String::AsciiValue(arguments[0]->ToString()));
  ERL_NIF_TERM F = enif_make_atom(script->env,*v8::String::AsciiValue(arguments[1]->ToString()));
  ERL_NIF_TERM A = js_to_term(script->env,arguments[2]);

  script->send(enif_make_tuple(script->env, 3, M, F, A));
  return v8::Integer::New(0); // FIXME
}

static void script_resource_destroy(ErlNifEnv* env, void* obj)
{
}

int load(ErlNifEnv *env, void** priv_data, ERL_NIF_TERM load_info)
{
  script_resource = enif_open_resource_type(env, NULL, "erlv8_resource", script_resource_destroy, (ErlNifResourceFlags) (ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER), NULL);
  v8::HandleScope handle_scope;

  v8::Handle<v8::FunctionTemplate> EcallF = v8::FunctionTemplate::New(CallFun);

  v8::Handle<v8::ObjectTemplate> erlang = v8::ObjectTemplate::New();
  erlang->Set(v8::String::New("__call__"),EcallF);

  global_template = v8::Persistent<v8::ObjectTemplate>::New(v8::ObjectTemplate::New());
  global_template->Set(v8::String::New("Erlang"),erlang);

  return 0;
};

void unload(ErlNifEnv *env, void* priv_data)
{
  global_template.Dispose();
};

ERL_NIF_INIT(erlv8_nif,nif_funcs,load,NULL,NULL,unload)
