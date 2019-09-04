
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