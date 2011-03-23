#include "erlv8.hh"

TickHandler(GCTickHandler) {
  while (!v8::V8::IdleNotification()) 
	;
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}
