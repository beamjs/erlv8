#include "erlv8.hh"

v8::Handle<v8::Object> extern_name_to_proto(VM * vm, char *name) {
  v8::Handle<v8::Object> proto;

  if (!strcmp(name,"num")) {
	proto = vm->external_proto_num;
  } else if (!strcmp(name,"atom")) {
	proto = vm->external_proto_atom;
  } else if (!strcmp(name,"bin")) {
	proto = vm->external_proto_bin;
  } else if (!strcmp(name,"ref")) {
	proto = vm->external_proto_ref;
  } else if (!strcmp(name,"fun")) {
	proto = vm->external_proto_fun;
  } else if (!strcmp(name,"port")) {
	proto = vm->external_proto_port;
  } else if (!strcmp(name,"pid")) {
	proto = vm->external_proto_pid;
  } else if (!strcmp(name,"tuple")) {
	proto = vm->external_proto_tuple;
  } else if (!strcmp(name,"list")) {
	proto = vm->external_proto_list;
  } else {
	proto = v8::Object::New();
  }

  return proto;
}

TickHandler(ExternProtoTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM extern_proto_ref = enif_make_copy(ref_env, tick_ref);
  char name[MAX_ATOM_LEN];
  unsigned len;

  enif_get_atom_length(vm->env, array[1], &len, ERL_NIF_LATIN1);
  enif_get_atom(vm->env,array[1],(char *)&name,len + 1, ERL_NIF_LATIN1);
  
  v8::Handle<v8::Object> proto = extern_name_to_proto(vm, name);

  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,extern_proto_ref),
						js_to_term(env,proto)));

  enif_free_env(ref_env);
  return DONE;
}

TickHandler(ExternalizeTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM extern_ref = enif_make_copy(ref_env, tick_ref);
  char name[MAX_ATOM_LEN];
  unsigned len;

  enif_get_atom_length(vm->env, array[1], &len, ERL_NIF_LATIN1);
  enif_get_atom(vm->env,array[1],(char *)&name,len + 1, ERL_NIF_LATIN1);
  
  v8::Handle<v8::Object> proto = extern_name_to_proto(vm, name);
  v8::Handle<v8::Object> obj = externalize_term(vm->extern_map, proto, array[2]);
  
  ERL_NIF_TERM resource_term;
	  
  val_res_t *ptr;
  ptr = (val_res_t *)enif_alloc_resource(val_resource, sizeof(val_res_t));
  ptr->val = v8::Persistent<v8::Object>::New(v8::Handle<v8::Object>::Cast(obj));
  ptr->ctx = v8::Persistent<v8::Context>::New(v8::Context::GetCurrent());
  resource_term = enif_make_resource(vm->env, ptr);
  enif_release_resource(ptr);

  SEND(vm->server,
	   enif_make_tuple3(env,
						enif_make_atom(env,"result"),
						enif_make_copy(env,extern_ref),
						enif_make_tuple3(env,
										 enif_make_atom(env, "erlv8_object"),
										 enif_make_copy(env, resource_term),
										 enif_make_pid(env, vm->server)
										 )));

  enif_free_env(ref_env);
  return DONE;
}
