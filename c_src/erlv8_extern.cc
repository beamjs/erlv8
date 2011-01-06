#include "erlv8.hh"

TickHandler(ExternProtoTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM extern_proto_ref = enif_make_copy(ref_env, vm->tick_ref);
  char name[MAX_ATOM_LEN];
  unsigned len;

  enif_get_atom_length(vm->env, array[1], &len, ERL_NIF_LATIN1);
  enif_get_atom(vm->env,array[1],(char *)&name,len + 1, ERL_NIF_LATIN1);
  
  v8::Handle<v8::Object> proto;
  
  if (!strcmp(name,"pid")) {
	proto = vm->external_proto_pid;
  } else if (!strcmp(name,"ref")) {
	proto = vm->external_proto_ref;
  }

  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,extern_proto_ref),
						js_to_term(env,proto)));

  enif_free_env(ref_env);
  return DONE;
}
