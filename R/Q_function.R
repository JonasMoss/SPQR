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
#' @param call A call object.
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


Q = function(call, quote = TRUE, .force = TRUE, .enclos = NULL) {

  call      = if(quote) substitute(call) else call

  if(!is.call(call)) {
    msg = paste0("The 'call' argument must be an object of class 'call'. Got ",
                 "an object of class '", class(call)[1], "'.")
    stop(msg)
  }

  name      = deparse(call[[1]])
  object    = getFunction(name)

  if(!rlang::is_closure(object)) {
    msg = paste0("The name '", name, "' does not match a closure.")
    stop(msg)
  }

  args      = as.list(match.call(object, call)[-1])
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


plot.default = Q(plot.default(type = "l"))
x = seq(-5, 20, by = 0.01)
dnormx = Q(dnorm(x = x, mean = 1, sd = 2))


plot(x, dnormx())
lines(x, dnormx(mean = 2))
lines(x, dnormx(mean = 6))
lines(x, dnormx(mean = 4))
lines(0:10, dnormx(0:10, mean = 8), type = "b")

library("magrittr")
density.default = Q(density.default(adjust = 2))
rnorm2 = Q(rnorm(n = 10000, mean = 2, sd = 2.1))

density2(rnorm2()) %>% plot
density2(rnorm2()*rnorm2(mean = 1)) %>% lines(col = "blue")

dnormy = function(x = x, mean = 1, sd = 2, log = TRUE) {
  dnorm(x, mean, sd, log)
}




arguments = function() as.list(match.call(sys.function(-1), sys.call(-1))[-1])

dnormy = function(x = x, mean = 1, sd = 2, log = TRUE) {
  arguments()
  formals()
  #dnorm(x, mean, sd, log)
}

hist.default = Q(hist.default(freq = FALSE, breaks = "FD"))
hist(rnorm(1000))

par("bty" = "l")


means = c(1, 2, 3)
function(x) sapply(means, function(.) eval(Q(dnorm(mean = .))))

Q2 = function(call, quote = TRUE, .force = TRUE, .enclos = NULL) {

  call = if(quote) substitute(call) else call
  name = deparse(call[[1]])
  object = getFunction(name)

  if(!is.null(.enclos)) environment(object) = .enclos

  args = as.list(match.call(object, call)[-1])

  if(.force) args = lapply(args, eval, envir = parent.frame(n = 3))

  f = function() NULL
  formals(f) = formals(object)

  for(arg in names(args)) formals(f)[[arg]] = args[[arg]]

  if(!is.null(.enclos)) environment(f) = .enclos

  f
}

Q2(hist.default(freq = FALSE, breaks = "FD"))

call = quote(hist(freq = FALSE, breaks = "FD"))

e = new.env()
e$z = 0
z = 1
g = function(x = z) x
environment(g) = e