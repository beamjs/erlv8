#include "erlv8.hh"

TickHandler(StopTickHandler) {
  result = v8::Undefined();
  return RETURN;
}
