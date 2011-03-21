#include "v8.h"

#include "erl_nif.h"

#include <pthread.h>

#include <iostream>
#include <cstring>
#include <cmath>
#include <map>
#include <queue>

#include <zmq.h>

using namespace std;
using namespace __gnu_cxx;

// This should be enough for our tags:
#define MAX_ATOM_LEN 32 

class VM; // fwd
typedef struct _vm_res_t { 
  VM * vm;
} vm_res_t;

typedef struct _val_res_t { 
  v8::Persistent<v8::Context> ctx;
  v8::Persistent<v8::Value> val;
} val_res_t;

typedef struct _term_ref_t {
  ErlNifEnv *env;
  ERL_NIF_TERM term;
} term_ref_t;

typedef struct _ctx_res_t { 
  v8::Persistent<v8::Context> ctx;
} ctx_res_t;


// Helpers
#define LHCS(ctx) v8::Locker locker; \
  v8::HandleScope handle_scope;					\
  v8::Context::Scope context_scope(ctx)

#define SEND(pid, code) \
  { \
	Send send = Send(pid); \
	ErlNifEnv * env = send.env; \
	send.send(code); \
  } 
//

struct cmp_erl_nif_term
{
  bool operator()(ERL_NIF_TERM a, ERL_NIF_TERM b)
   {
	 return enif_compare(a,b) < 0;
   }
};


// Statics
extern v8::Persistent<v8::ObjectTemplate> global_template;
extern v8::Persistent<v8::ObjectTemplate> external_template;
extern v8::Persistent<v8::FunctionTemplate> empty_constructor;
extern v8::Persistent<v8::String> string__erlv8__;

extern ErlNifResourceType * vm_resource;
extern ErlNifResourceType * val_resource;
extern ErlNifResourceType * ctx_resource;

extern void *zmq_context;

//

struct Tick {
  ErlNifEnv * env;
  ERL_NIF_TERM tick;
  ERL_NIF_TERM ref;
};

// VM
class VM {
public:
  v8::Persistent<v8::Context> context;

  v8::Persistent<v8::Object> external_proto_num;
  v8::Persistent<v8::Object> external_proto_atom;
  v8::Persistent<v8::Object> external_proto_bin;
  v8::Persistent<v8::Object> external_proto_ref;
  v8::Persistent<v8::Object> external_proto_fun;
  v8::Persistent<v8::Object> external_proto_port;
  v8::Persistent<v8::Object> external_proto_pid;
  v8::Persistent<v8::Object> external_proto_tuple;
  v8::Persistent<v8::Object> external_proto_list;

  ErlNifPid *server;
  ErlNifEnv *env;

  void * push_socket;
  void * ticker_push_socket;
  void * pull_socket;

  queue<Tick> pop_ticks;

  ErlNifTid tid;
  
  vm_res_t * resource;

  map<ERL_NIF_TERM, v8::Handle<v8::FunctionTemplate>, cmp_erl_nif_term> fun_map;
  map<ERL_NIF_TERM, v8::Handle<v8::Object>, cmp_erl_nif_term> extern_map;

  VM();
  ~VM();
  void run();
  v8::Handle<v8::Value> ticker(ERL_NIF_TERM ref);

};

enum TickHandlerResolution { DONE, RETURN, NEXT };

#define TickHandler(name) extern TickHandlerResolution name(VM * vm, char * tick_name, ERL_NIF_TERM tick, ERL_NIF_TERM tick_ref, ERL_NIF_TERM ref, int arity, const ERL_NIF_TERM * array, v8::Handle<v8::Value>& result)

TickHandler(StopTickHandler);
TickHandler(ResultTickHandler);
TickHandler(CallTickHandler);
TickHandler(InstantiateTickHandler);
TickHandler(TaintTickHandler);
TickHandler(GetTickHandler);
TickHandler(GetProtoTickHandler);
TickHandler(GetHiddenTickHandler);
TickHandler(SetTickHandler);
TickHandler(SetProtoTickHandler);
TickHandler(SetHiddenTickHandler);
TickHandler(ProplistTickHandler);
TickHandler(ListTickHandler);
TickHandler(ScriptTickHandler);
TickHandler(GCTickHandler);
TickHandler(ToStringTickHandler);
TickHandler(ToDetailStringTickHandler);
TickHandler(ExternProtoTickHandler);
TickHandler(ExternalizeTickHandler);
TickHandler(UnknownTickHandler);
TickHandler(InternalCountTickHandler);
TickHandler(SetInternalTickHandler);
TickHandler(GetInternalTickHandler);

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




v8::Handle<v8::Value> term_to_js(ErlNifEnv *env, ERL_NIF_TERM term);
ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val);
ERL_NIF_TERM external_to_term(v8::Handle<v8::Value> val);
v8::Handle<v8::Value> term_to_external(ERL_NIF_TERM term);
v8::PropertyAttribute term_to_property_attribute(ErlNifEnv * env, ERL_NIF_TERM term);
int enif_is_proplist(ErlNifEnv * env, ERL_NIF_TERM term);
v8::PropertyAttribute term_to_property_attribute(ErlNifEnv * env, ERL_NIF_TERM term);
v8::Handle<v8::Value> term_to_external(ERL_NIF_TERM term);
ERL_NIF_TERM external_to_term(v8::Handle<v8::Value> val);
v8::Handle<v8::Object> externalize_term(map<ERL_NIF_TERM, v8::Handle<v8::Object>, cmp_erl_nif_term> cache, v8::Handle<v8::Object> proto, ERL_NIF_TERM term);
v8::Handle<v8::Value> term_to_js(ErlNifEnv *env, ERL_NIF_TERM term);
ERL_NIF_TERM js_to_term(ErlNifEnv *env, v8::Handle<v8::Value> val);
v8::Handle<v8::Value> WrapFun(const v8::Arguments &arguments);
v8::Handle<v8::Value> EmptyFun(const v8::Arguments &arguments);

v8::Handle<v8::Object> extern_name_to_proto(VM * vm, char *name);

// Debugging

#ifdef ERLV8_DEBUG
#define DEBUG(pid,name,code) SEND(pid, enif_make_tuple3(env, enif_make_atom(env,"DEBUG"), name, code))
#else
#define DEBUG(pid,name,code)
#endif

