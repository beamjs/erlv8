#include "erlv8.hh"

TickHandler(ResultTickHandler) {
 if (((unsigned long) ref) &&
	  (enif_is_identical(array[1],ref))) { // this is our result
	result = term_to_js(vm->env,array[2]);
	return RETURN;
  } else {
   zmq_msg_t tick_msg;
   Tick * newtick = (Tick *) malloc(sizeof(Tick));
   int e;
   
   newtick->env = enif_alloc_env();
   newtick->tick = enif_make_copy(newtick->env, tick);
   newtick->ref = enif_make_copy(newtick->env, tick_ref);
   
   zmq_msg_init_data(&tick_msg, newtick, sizeof(Tick), free_tick, NULL);
   do {
	 e = zmq_send(vm->ticker_push_socket, &tick_msg, ZMQ_NOBLOCK);
   } while (e == EAGAIN);
   zmq_msg_close(&tick_msg);
   
   return DONE;
  }
}
