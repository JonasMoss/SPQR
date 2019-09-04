R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call
  e = arguments_available(call, env = parent.frame())
  eval(call, envir = e)
}


# And other one: This one passes the same arguments to the underlying function,
# but creates a wrapper environment where the arguments can be found.

#' Checks if a call references a name.

call_uses_name = function(call, name) {

  args = as.list(call)[-1]

  for(i in which(sapply(args, is.name)))
    if(name == args[[i]])
      return(TRUE)

  val = FALSE
  for(i in which(sapply(args, is.call)))
    val = val | call_uses_name(args[[i]], name)

  return(val)
}



# call = quote(list(sin(x, cos(w, exp(v))), cos(sin(y)), z))
# name = quote(v)
# call_uses_name(call, name)

#' Checks if all the arguments of a call are bound.

arguments_available = function(call, env = parent.frame()) {
  args = as.list(call)[-1]
  formals = lapply(names(args), function(arg) parse(text = arg)[[1]])
  names(formals) = names(args)

  name_indices  = sapply(args, is.name)
  call_indices  = sapply(args, is.call)

  call_args  = args[which(call_indices)]

  e = as.environment(args[!name_indices & !call_indices])
  parent.env(e) = env

  # Arguments with calls are included in two cases:
  # 1.) They are self-evaluating, without reference to the other names.
  # 2.) They are referenced name by another variable.
  for(arg in names(call_args)) {
    include = TRUE

    for(name in formals)
      # Now we
      if(call_uses_name(call_args[[arg]], name)) {
        include = FALSE
        break
      }

    if(include) do.call(delayedAssign, list(arg, call_args[[arg]], e, e))
  }

  # The 'name' indices must also be added.

  e
}


#
# R = function(call, quote = TRUE) {
#   call = if(quote) substitute(call) else call
#   f    = deparse(call[[1]])
#   args = as.list(call)[-1]
#   e    = new.env()
#
#   for(arg in names(args)) do.call(delayedAssign, list(arg, args[[arg]], e, e))
#
#   do.call(f, as.list(e))
# }
#
# n = 100

R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call

  fn = deparse(call[[1]])
  args = as.list(call[-1])

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

  unlockBinding(fn, enclosing_env)
  environment(enclosing_env[[fn]]) = new_env

  on.exit({
    environment(enclosing_env[[fn]]) = defining_env
    #lockBinding(fn, enclosing_env)
  })

  eval(call, calling_env)
}
