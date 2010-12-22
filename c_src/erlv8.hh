#include "v8.h"

#include "erl_nif.h"

#include <iostream>
#include <cstring>
#include <cmath>

using namespace std;
using namespace __gnu_cxx;


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


// Statics
extern v8::Persistent<v8::ObjectTemplate> global_template;
extern ErlNifResourceType * vm_resource;
extern ErlNifResourceType * val_resource;
extern ErlNifResourceType * ctx_resource;
//

// VM
class VM {
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
  
  vm_res_t * resource;
  VM();
  ~VM();
  void requestTick();
  void waitForTick();
  void run();
  v8::Handle<v8::Value> ticker(ERL_NIF_TERM ref);

};

enum TickHandlerResolution { DONE, RETURN, NEXT };

#define TickHandler(name) TickHandlerResolution name(VM * vm, char * tick_name, ERL_NIF_TERM ref, int arity, const ERL_NIF_TERM * array, v8::Handle<v8::Value>& result)

TickHandler(StopTickHandler);
TickHandler(ResultTickHandler);
TickHandler(CallTickHandler);
TickHandler(GetTickHandler);
TickHandler(SetTickHandler);
TickHandler(ScriptTickHandler);
TickHandler(UnknownTickHandler);

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
