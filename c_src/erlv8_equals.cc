#include "erlv8.hh"

TickHandler(EqualsTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM equals_ref = enif_make_copy(ref_env, tick_ref);
  val_res_t *res1; 
  val_res_t *res2; 
  bool bres;

  if ((enif_get_resource(vm->env,array[1],val_resource,(void **)(&res1))) &&
	  (enif_get_resource(vm->env,array[2],val_resource,(void **)(&res2)))) {
	LHCS(vm->isolate, res1->ctx);
    bres = res1->val->ToObject()->Equals(res2->val->ToObject());
  } else {
    LHCS(vm->isolate, vm->context);
    bres = term_to_js(vm->context, vm->isolate, vm->env,array[1])->Equals(term_to_js(vm->context, vm->isolate, vm->env,array[2]));
  };

  SEND(vm->server,
       enif_make_tuple3(env,
                        enif_make_atom(env,"result"),
                        enif_make_copy(env,equals_ref),
                        enif_make_atom(env, bres ? "true" : "false")));

  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(StrictEqualsTickHandler) {
  ErlNifEnv *ref_env = enif_alloc_env();
  ERL_NIF_TERM equals_ref = enif_make_copy(ref_env, tick_ref);
  val_res_t *res1; 
  val_res_t *res2; 
  bool bres;

  if ((enif_get_resource(vm->env,array[1],val_resource,(void **)(&res1))) &&
	  (enif_get_resource(vm->env,array[2],val_resource,(void **)(&res2)))) {
	LHCS(vm->isolate, res1->ctx);
    bres = res1->val->ToObject()->StrictEquals(res2->val->ToObject());
  } else {
    LHCS(vm->isolate, vm->context);
    bres = term_to_js(vm->context, vm->isolate, vm->env,array[1])->StrictEquals(term_to_js(vm->context, vm->isolate, vm->env,array[2]));
  };

  SEND(vm->server,
       enif_make_tuple3(env,
                        enif_make_atom(env,"result"),
                        enif_make_copy(env,equals_ref),
                        enif_make_atom(env, bres ? "true" : "false")));

  enif_free_env(ref_env);
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
