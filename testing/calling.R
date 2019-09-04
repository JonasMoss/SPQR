#' Create a function from an partial function call
#'
#' Partial function function application is usually done by using the
#' \code{function} keyword to define a new function with some arguments
#' pre-specified. This function allows you to make a partially applied function
#' directly from a partial function call.
#'
#' @param call A partial function call.
#' @param quote Logical; If \code{TRUE}, the call is quoted.
#' @return A function.

P = function(call, quote = TRUE) {
  
  f_call      = if(quote) substitute(call) else call
  
  if(!is.call(f_call)) {
    msg = paste0("The 'call' argument must be an object of class 'call'. Got ",
                 "an object of class '", class(f_call)[1], "'.")
    stop(msg)
  }
  
  f_name      = f_call[[1]]
  f_object    = getFunction(deparse(f_name))
  f_arguments = as.list(match.call(f_object, f_call)[-1])
  f_formals   = formals(f_object)

  new_f = function() {
    new_f_arguments = as.list(match.call())[-1]
    eval(expr  = as.call(c(f_name, new_f_arguments, f_arguments)),
         envir = parent.frame())
  }

  formals(new_f) = f_formals[setdiff(names(f_formals), names(f_arguments))]
  new_f
}


h = function(call) is.call(call)

call = quote(lm(mpg ~ cyl + wt, model = FALSE, zoop = "g"))
P(call, quote = FALSE)(mtcars)
P(lm(mpg ~ cyl + wt, model = FALSE, zoop = "g"))(mtcars)

P(lm(mpg ~ cyl + wt, model = FALSE))(mtcars, x = TRUE)


#' Change default arguments of a function with a partial call
#' 
#' @param call A call object.
#' @return A new function.


Q = function(call, quote = TRUE) {
  
  call      = if(quote) substitute(call) else call
  
  if(!is.call(call)) {
    msg = paste0("The 'call' argument must be an object of class 'call'. Got ",
                 "an object of class '", class(call)[1], "'.")
    stop(msg)
  }
  
  name      = call[[1]]
  object    = getFunction(deparse(name))
  args      = as.list(match.call(object, call)[-1])
  formals   = formals(object)
  
  for(arg in names(args)) formals[[arg]] = args[[arg]]

  formals(object) = formals 
  object
}


Q(lm(data = mtcars)) -> lmcars

list(x = 1, y = x, z = x)

is_nothing      = function(x) identical(x, bquote())
nothing_indices = function(l) sapply(l, is_nothing)
nothing_names   = function(l) names(which(nothing_indices(l)))
nothing_along   = function(l) lapply(l, function(x) bquote())

#' Allow self-referential arguments in functions
#' 
#' @param call A function call.
#' @param quote Logical; if \code{TRUE}, the supplied \code{call} is interpreted
#' as a quote, so \code{substitute} is applied.
#' @return The evaluated function call with the self-refering arguments 
#' evaluated.
#' @examples 
#'     R(plot(y = 1:10, x = y^2))
#'     R(plot(x = y^2, y = 1:10))
#'     R(plot(x = 1:10, y = x^2, xlab = x, ylab = y, main = xlab)) 
#'     
#'     simulated = R(data.frame(y  = 2*x1 + 3*x2 + rnorm(100),
#'                              x1 = rnorm(100),
#'                              x2 = rnorm(100)))

R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call
  f    = deparse(call[[1]])
  args = as.list(call)[-1]
  e    = new.env()
  
  for(arg in names(args)) do.call(delayedAssign, list(arg, args[[arg]], e, e))
  
  do.call(f, as.list(e))
}

# This is another variant.

R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call
  f    = deparse(call[[1]])
  args = as.list(call)[-1]
  empty_args = lapply(names(args), function(arg) parse(text = arg)[[1]])
  names(empty_args) = empty_args
  
  fn = function() eval(as.call(c(call[[1]], empty_args)))

  formals(fn) = args
  
  fn()
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

R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call
  args = as.list(call[-1])/
  e    = arguments_available(call, env = parent.frame())
  eval(call, envir = e)
}



# R = function(call, quote = TRUE) {
#   call = if(quote) substitute(call) else call
#   args = as.list(call[-1])
#   e = new.env()
#   for(arg in names(args)) do.call(delayedAssign, list(arg, args[[arg]], e, e))
#   eval(expr = call, envir = e)
# }

plot(x = 1:10, y = x^2, xlab = x, ylab = y, main = 2)
call = quote(plot(x = 1:10, y = x^2, xlab = x, ylab = y, main = 2))

R(plot(x = 1:10, y = x^2, 
       xlab = x, 
       ylab = y,
       main = xlab))
R(plot(y = 1:10, x = y^2))
R(plot(x = y^2, y = 1:10))

R(data.frame(y  = 100*x1 + rnorm(100),
             x1 = rnorm(100))) %>%
  ggplot2::ggplot(ggplot2::aes(x = x1, y = y)) + 
  ggplot2::geom_point() + 
  ggplot2::geom_line()
