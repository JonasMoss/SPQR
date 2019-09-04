#' Change default arguments of a closure with a partial call
#'
#' This function takes a partial call and returns a copy of the function
#' in the call with default arguments replaced by the arguments in the partial
#' call.
#'
#' The enclosing environment of the function is not changed by default, and
#' changing it will typically break the function. The \code{.force} argument is
#' set to \code{TRUE} by default, since the function will attempt to evaluate
#' its default arguments in its enclosing context, where the supplied values are
#' unlikely to exist.
#'
#' @export
#' @param .call A call object.
#' @param .quote Logical; If \code{TRUE}, quotes the \code{call} argument.
#' @param .force Logical; If \code{TRUE}, the arguments in the partial call are
#' forced.
#' @param .enclos The enclosing environment of the function. If \code{NULL}, the
#' enclosing environment is not changed.
#' @return A new closure.
#' @examples
#'    ## Play around with normals.
#'    dnorm2 = Q(dnorm(mean = 1, sd = 2))
#'    dnorm2(1) == dnorm(1, mean = 1, sd = 2)
#'    z = 1
#'    dnorm3 = Q(dnorm(mean = z, sd = 2))
#'    dnorm2(1) == dnorm(1, mean = z, sd = 2)
#'
#'    ## Change plot.default to use type = "l" and lwd = 1.5 as default
#'    plot.default = Q(plot.default(type = "l"))
#'    par("lwd" = 1.5)
#'
#'    ## Automatically add x to plots.
#'
#'


Q = function(.call, .quote = TRUE, .force = TRUE, .enclos = NULL) {

  .call = if(.quote) substitute(.call) else .call

  if(!is.call(.call)) {
    msg = paste0("The '.call' argument must be an object of class 'call'. Got ",
                 "an object of class '", class(.call)[1], "'.")
    stop(msg)
  }

  name = deparse(.call[[1]])
  object = methods::getFunction(name)

  if(!rlang::is_closure(object)) {
    msg = paste0("The name '", name, "' does not match a closure.")
    stop(msg)
  }

  args = as.list(match.call(object, .call)[-1])
  if(.force) args = lapply(args, eval, envir = parent.frame(n = 3))

  formals   = formals(object)

  if(any(is.na(match(names(args), names(formals))))) {
    msg = paste0("The arguments do not match the formals.")
    stop(msg)
  }

  for(arg in names(args)) formals[[arg]] = args[[arg]]

  formals(object) = formals

  if(!is.null(.enclos)) environment(object) = .enclos

  object

}
