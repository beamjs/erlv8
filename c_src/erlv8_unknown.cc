#include "erlv8.hh"

TickHandler(UnknownTickHandler) {
  zmq_msg_t tick_msg;
  Tick * newtick = (Tick *) malloc(sizeof(Tick));
  int e;
  
  newtick->env = enif_alloc_env();
  newtick->tick = enif_make_copy(newtick->env, array[2]);
  newtick->ref = enif_make_copy(newtick->env, array[1]);
  
  zmq_msg_init_data(&tick_msg, newtick, sizeof(Tick), free_tick, NULL);
  do {
	e = zmq_send(vm->push_socket, &tick_msg, ZMQ_NOBLOCK);
  } while (e == EAGAIN);
  zmq_msg_close(&tick_msg);
  
  return DONE;
}

