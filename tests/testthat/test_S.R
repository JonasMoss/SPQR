context("S function")

x = 3
f = function() x^2
g = function() {
    x = 10
    S(f())
}

expect_equal(g(), 100)
expect_equal(f(), 9)
expect_equal(S(f()), 9)
