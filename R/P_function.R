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
