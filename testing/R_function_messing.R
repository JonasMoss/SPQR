
df = R(data.frame(y = 5 + 3*x1 + 6*x2 + rnorm(n),
                  x1 = rnorm(n),
                  x2 = rnorm(n)))
lm(y ~ x1 + x2, data = df)
R(plot(x = seq(-1, 1, by = 0.01), y = dnorm(x, 0, 3)))

f = function(x, y) environment()

R(f(x = seq(-1, 1, by = 0.01), y = dnorm(x, 0, 3)))