#include "erlv8.hh"

TickHandler(DeleteTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
    LHCS(vm->isolate, obj_res->ctx);
    v8::Handle<v8::Value> key = term_to_js(obj_res->ctx,vm->isolate,vm->env,array[2]);
    if (key->IsString()) {
      obj_res->val->ToObject()->Delete(key->ToString());
    } else if (key->IsNumber()) {
      obj_res->val->ToObject()->Delete(key->Uint32Value());
    }
    
    SEND(vm->server,
	 enif_make_tuple3(env,
			  enif_make_atom(env,"result"),
			  enif_make_copy(env,tick_ref),
			  enif_make_atom(env, "ok")));
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

