#' Extracts variables from a call
#'
#' @param call A call
#' @param .standardise Should the calls be standardized?
#' @return A list containing all the variables referenced in the call.

variables = function(call, .standardise = TRUE) {
  stopifnot(is.call(call))
  if(.standardise) call = rlang::call_standardise(call)
  unlist(lapply(as.list(call)[-1], function(arg) {
    if(!is.call(arg)) arg else variables(arg)
  }))
}

#' Variant of do.call that preserves argument names.
#'
#' The function \code{base::do.call} does not always work as intended. This is
#' because it changes
#'
#' @param .fn Function to call.
#' @param .args List of arguments to \code{.fn}.
#' @param ... Further arguments to \code{.fn}.
#' @param .env The environment where the call is to be evaluated.
#' @return The effect of calling \code{.fn} with the supplied arguments in the
#' specified environment.

do_call = function(.fn, .args = NULL, ..., .env = parent.frame()) {
  call = as.call(c(.fn, .args, alist2(...)))
  eval(call, envir = .env)
}

#' Make lazy list from arguments
#'
#' Works with passed \code{...} parameters.
#' @param ... Parameters to put into the list.
#' @return A lazy list.

alist2 = function(...) as.list(substitute((...)))[-1]

#' Adds named elements to a list when they are not there already.
#'
#' @param input List. The input list to manipulate.
#' @param ... Key value pairs to add to the list provided the key is not already
#' used.
#' @param .lazy Logical; Should the \code{value}s be evaluated?
#' @return A modified list.

add_elements = function(input, ..., .lazy = FALSE) {
  dots = if(.lazy) alist2(...) else list(...)
  names = names(dots)
  N = length(names)

  for(i in 1:N) if(is.null(input[[names[i]]])) input[[names[i]]] = dots[[i]]

  input
}


#' Plot with \code{type = "l", bty = "l", lwd = 1.5} as standard arguments.
#'
#' @param ... Arguments passed to \code{plot}

lplot = function(...) {
  dots = alist2(...)
  if(is.null(dots$type)) dots$type = "l"
  if(is.null(dots$bty)) dots$bty = "l"
  if(is.null(dots$lwd)) dots$lwd = 1.5
  do_call(graphics::plot, dots)
}

lapply2 = function(X, FUN, ...) {

  FUN = substitute(FUN)

  lapply(X, function (x) {
    call = c(FUN, x, alist2(...))
    eval(as.call(call), envir = parent.frame())
  })

}

where = function(name, env = parent.frame()) {
  if (identical(env, emptyenv())) stop("Can't find ", name, call. = FALSE)
  if (exists(name, envir = env, inherits = FALSE)) return(env)
  where(name, parent.env(env))
}