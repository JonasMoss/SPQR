formula = mpg ~ cyl + wt
data = mtcars
do.call(sys.parents, list())

formulas = list(mpg ~ cyl + wt,
                mpg ~ cyl,
                mpg ~ wt)

lapply2(formulas, lm, data = mtcars)
lapply(formulas, lm, data = mtcars)
lapply2(formulas, function(x, data) x, data = mtcars)

f = alist(sum, prod)

y = vector("list", length(f))
x = 1:10

for(i in 1:length(f)) {
  y[[i]] = function(y) f[[i]](y)
}

lapply2(f, identity)
lapply(f, identity)

y[[1]](x)
y[[2]](x)


