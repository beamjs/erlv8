#include "erlv8.hh"

TickHandler(StopTickHandler) {
  v8::HandleScope handle_scope;
  TickHandlerResolution result;
  result.value = v8::Undefined();
  result.type = RETURN;
  return result;
}
