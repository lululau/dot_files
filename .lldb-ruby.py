import lldb

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f ruby.rb_backtrace rb_backtrace')
    debugger.HandleCommand('command script add -f ruby.rb_eval rb_eval')

def rb_backtrace(debugger, command, result, internal_dict):
    debugger.HandleCommand('expr (void)rb_backtrace()')

def rb_eval(debugger, command, result, internal_dict):
    debugger.HandleCommand('expr (void *)rb_p((void *)rb_eval_string_protect(%s, (int *) 0))' % command)
