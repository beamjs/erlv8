#include "erlv8.hh"

TickHandler(UnknownTickHandler) {
  TickHandlerResolution result;
  result.type = DONE;
  return result;
}

