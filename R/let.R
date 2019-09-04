## This file contains R-variants of let, let* and letrec
##



















"What? How did you arrive at that value?

These two formulas are exactly the same, as multiplication is associative - it doesn't care about parentheses. For instance (a*b)*c = a*(b*c). See e.g. https://en.wikipedia.org/wiki/Associative_property Moreover, there is nothing to figure out here. His r_to_t function is not the inverse of his t_to_r function, and vice versa. They are two different ways to convert between t and r values, where the second option doesn't respect signs. Look at my two comments above to see the correct inverses.

There is no "true" conversion between r and t (or d), as they are like apples and oranges. Correlations and t-values are just simply fundamentally different.

Uli's two formulas are probably just two reasonable but competing ways to convert between effect size measures."



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

#' Extracts variables from a call
#'
#' @param call A call
#' @param .standardise Should the calls be standardized?
#' @param .unlist Should the variables be unlisted?
#' @return A list containing all the variables referenced in the call.

variables = function(call, .standardise = TRUE, .unlist = FALSE) {
  stopifnot(is.call(call))
  if(.standardise) call = rlang::call_standardise(call)

  vars = lapply(as.list(call)[-1], function(arg) {
    if(!is.call(arg)) arg else variables(arg)
  })

  if(.unlist) unlist(vars) else vars
}




R = function(call, quote = TRUE) {
  call = if(quote) substitute(call) else call

  fn = deparse(call[[1]])
  args = as.list(call[-1])
  variables = variables(call, .standardise = FALSE, .unlist = TRUE)
  names = unique(Filter(is.name, variables))


  #
  # calling_env   = parent.frame()
  # enclosing_env = where(fn, env = calling_env)
  # defining_env  = environment(enclosing_env[[fn]])
  # new_env       = new.env(parent = calling_env)
  #
  # for(arg in names(args)) {
  #   .args = list(x = arg,
  #                value = args[[arg]],
  #                eval.env = new_env,
  #                assign.env = new_env)
  #   do.call(delayedAssign, .args)
  #
  # }
  #
  # unlockBinding(fn, enclosing_env)
  # environment(enclosing_env[[fn]]) = new_env
  #
  # on.exit({
  #   environment(enclosing_env[[fn]]) = defining_env
  #   #lockBinding(fn, enclosing_env)
  # })
  #
  # eval(call, calling_env)
}

n = 100
r = .3
t = 3.113247



t = r * sqrt(n-2) / (1-r^2)
t = r * sqrt(n-2) / (1-r^2)
(sqrt(n + 4*t^2 - 2) - sqrt(n-2))/(2*t)


df = R(data.frame(y = 5 + 3*x1 + 6*x2 + rnorm(n),
                  x1 = rnorm(n),
                  x2 = rnorm(n)))

call = quote(data.frame(y = 5 + 3*x1 + 6*x2 + rnorm(n),
                        x1 = rnorm(n),
                        x2 = rnorm(n)))
uniques(Filter(is.name, variables(call, .standardise = FALSE, .unlist = TRUE)))

lm(y ~ x1 + x2, data = df)
R(plot(x = seq(-1, 1, by = 0.01), y = dnorm(x, 0, 3)))

f = function(x, y) environment()

R(f(x = seq(-1, 1, by = 0.01), y = dnorm(x, 0, 3)))


call = quote(f(g(x = y), 1, 2, z = 55, h(s(b = "s"))))
variables(call)


fum = function(..., na.rm) sum(..., na.rm)

match.call(fum, quote(sum(na.rm = FALSE, 1, 2, 3)))
rlang::call_standardise(quote(sum(na.rm = FALSE, 1, 2, 3)))


call = quote(sum(na.rm = FALSE, 1, 2, (x*5)^2, x + z + (w*(v/(b - a)))))
call = quote((x*5)^2)
variables(call)
variables(call, .standardise = FALSE)