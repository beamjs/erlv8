#include "erlv8.hh"

TickHandler(GetTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
    LHCS(vm->isolate, obj_res->ctx);
    v8::Local<v8::Value> get_result = obj_res->val->ToObject()->Get(term_to_js(obj_res->ctx,vm->isolate, vm->env,array[2]));
    
    SEND(vm->server,
	 enif_make_tuple3(env,
			  enif_make_atom(env,"result"),
			  enif_make_copy(env,tick_ref),
			  js_to_term(obj_res->ctx, vm->isolate, env,get_result)));
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(GetProtoTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
    LHCS(vm->isolate, obj_res->ctx);
    v8::Local<v8::Value> get_result = obj_res->val->ToObject()->GetPrototype();
    
    SEND(vm->server,
	 enif_make_tuple3(env,
			  enif_make_atom(env,"result"),
			  enif_make_copy(env,tick_ref),
			  js_to_term(obj_res->ctx, vm->isolate, env,get_result)));
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(GetHiddenTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
    LHCS(vm->isolate, obj_res->ctx);
    v8::Local<v8::Value> get_result = obj_res->val->ToObject()->GetHiddenValue(term_to_js(obj_res->ctx,vm->isolate,vm->env,array[2])->ToString());
    
    SEND(vm->server,
	 enif_make_tuple3(env,
			  enif_make_atom(env,"result"),
			  enif_make_copy(env,tick_ref),
			  js_to_term(obj_res->ctx, vm->isolate, env,get_result)));
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

TickHandler(GetInternalTickHandler) {
  val_res_t *obj_res;
  if (enif_get_resource(vm->env,array[1],val_resource,(void **)(&obj_res))) {
    LHCS(vm->isolate, obj_res->ctx);
	
    int index;
    enif_get_int(vm->env, array[2], &index);
    
    if (index < 0 || (index + 1 > obj_res->val->ToObject()->InternalFieldCount())) {
      SEND(vm->server,
	   enif_make_tuple3(env,
			    enif_make_atom(env,"result"),
			    enif_make_copy(env,tick_ref),
			    enif_make_atom(env,"error")));
    } else {
      
      v8::Local<v8::Value> get_result = obj_res->val->ToObject()->GetInternalField(index);
      
      if (get_result->IsExternal()) {
	SEND(vm->server,
	     enif_make_tuple3(env,
			      enif_make_atom(env,"result"),
			      enif_make_copy(env,tick_ref),
			      external_to_term(get_result)));
      } else {
	SEND(vm->server,
	     enif_make_tuple3(env,
			      enif_make_atom(env,"result"),
			      enif_make_copy(env,tick_ref),
			      js_to_term(obj_res->ctx, vm->isolate, env,get_result)));
      }
      
    }
  }
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
