context('unmap_categories')

test_that('empty arguments', {
    expect_identical(unmap_categories(list(), list()), list())
    expect_identical(unmap_categories(data.frame(), list()), data.frame())
})

test_that('logical', {
    # as factor
    x <- as.factor(c(F,T))
    x_map <- setNames(c(F,T), x[1:2])
    expect_identical(unmap_categories(list(x=rep(x, each=2)), list(x=x_map)),
                     list(x=rep(c(F,T), each=2)))
})

test_that('integer', {
    # as factor
    x <- as.factor(1:3)
    x_map <- setNames(1:3, x[1:3])
    expect_identical(unmap_categories(list(x=rep(x, each=2)), list(x=x_map)),
                     list(x=rep(1:3, each=2)))
})

test_that('numeric', {
    # as factor
    x <- as.factor(0.5 + 1:3)
    x_map <- setNames(0.5 + 1:3, x[1:3])
    expect_identical(unmap_categories(list(x=rep(x, each=2)), list(x=x_map)),
                     list(x=rep(0.5 + 1:3, each=2)))
})

test_that('preserves names', {
    # as factor
    x <- setNames(letters[1:3], LETTERS[1:3])
    x_map <- setNames(1:3, letters[1:3])
    expect_identical(names(unmap_categories(list(x=x), list(x=x_map))$x),
                     names(x))
})

