R = function(call, .quote = TRUE) {
  .call = if(.quote) substitute(.call) else .call

  fn = deparse(.call[[1]])
  args = as.list(.call[-1])

  calling_env   = parent.frame()
  enclosing_env = where(fn, env = calling_env)
  defining_env  = environment(enclosing_env[[fn]])
  new_env       = new.env(parent = calling_env)

  for(arg in names(args)) {
    .args = list(x = arg,
                 value = args[[arg]],
                 eval.env = new_env,
                 assign.env = new_env)
    do.call(delayedAssign, .args)

  }

  environment(enclosing_env[[fn]]) = new_env

  on.exit({
    environment(enclosing_env[[fn]]) = defining_env
  })

  eval(call, calling_env)
}
