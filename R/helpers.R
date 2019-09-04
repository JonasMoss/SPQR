#' Checks if a call references a name.
#'
#' @param call A call.
#' @param name A name.

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

#' Checks if all the arguments of a call are bound.
#'
#' @param call A call.
#' @param env An environment.
#' @return e

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

      if(call_uses_name(call_args[[arg]], name)) {
        include = FALSE
        break
      }

    if(include) do.call(delayedAssign, list(arg, call_args[[arg]], e, e))
  }

  # The 'name' indices must also be added.

  e
}
