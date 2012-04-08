#include "erlv8.hh"

TickHandler(StopTickHandler) {
  TRACE("(%p) stop - 1\n", vm->isolate);
  v8::Isolate::Scope iscope(vm->isolate);
  TRACE("(%p) stop - 2\n", vm->isolate);
  v8::HandleScope handle_scope;
  TRACE("(%p) stop - 3\n", vm->isolate);
  TickHandlerResolution result;
  TRACE("(%p) stop - 4\n", vm->isolate);
  v8::V8::TerminateExecution();  
  TRACE("(%p) stop - 5\n", vm->isolate);
  result.value = v8::Undefined();
  TRACE("(%p) stop - 6\n", vm->isolate);
  result.type = RETURN;
  TRACE("(%p) stop - 7\n", vm->isolate);
  return result;
}
