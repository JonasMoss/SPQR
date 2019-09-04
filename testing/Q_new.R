Q = function(call, quote = TRUE, .force = TRUE, .enclos = NULL) {

  call = if(quote) substitute(call) else call
  name = deparse(call[[1]])
  object = getFunction(name)

  if(!is.null(.enclos)) environment(object) = .enclos

  args = as.list(match.call(object, call)[-1])

  if(.force) args = lapply(args, eval, envir = parent.frame(n = 3))

  for(arg in names(args)) formals(object)[[arg]] = args[[arg]]

  if(!is.null(.enclos)) environment(object) = .enclos

  object
}

