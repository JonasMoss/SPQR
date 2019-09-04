#' Evaluates a call as if its function was defined in a specified environment
#'
#' When a name is encountered in the body of a function, the search path
#' for that name is given by the defining (or enclosing) environment of the
#' function. This is good behaviour, since it allows simple reasoning about how
#' a function should behave: If two calls to a function defined in a constant
#' environment \code{e} yield different results, this must be because they are
#' given different arguments.
#'
#' Sometimes, a function is defined to make messy code more readable, but is
#' only intended to be used once. In these cases, you might want to call the
#' function as if it was defined in the body of the work you're currently
#' working with. And that is what this function does for you.
#'
#' @export
#' @param call The function call to evaluate.
#' @param env The environment where the function is defined. Defaults to
#' \code{parent.frame}, which makes the defining environment equal to the
#' current environment.
#' @param quote Logical; If \code{TRUE}, substitutes the argument.
#' @return The value of the function call when evaluated in the specified
#' environment.
#' @examples
#'    x = 3
#'    f = function() x^2
#'    g = function() {
#'      x = 10
#'      S(f())
#'    }
#'    g() # Evalutes to 100
#'    f() # Evalutes to 9
#'    S(f()) # Evaluates 9

S = function(call, env = parent.frame(), quote = TRUE) {

  call = if(quote) substitute(call) else call

  fn = deparse(call[[1]])

  calling_env   = parent.frame()
  enclosing_env = where(fn, env = calling_env)
  defining_env  = environment(enclosing_env[[fn]])

  unlockBinding(fn, enclosing_env)
  environment(enclosing_env[[fn]]) = env

  on.exit({
    environment(enclosing_env[[fn]]) = defining_env
    lockBinding(fn, enclosing_env)
  })

  eval(call, calling_env)

}
