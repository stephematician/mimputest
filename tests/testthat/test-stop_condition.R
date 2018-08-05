context('stop_condition')

test_that('both null', {
    expect_true(stop_condition(NULL, NULL))
})

test_that('previous null', {
    expect_false(stop_condition(NULL, 0))
})

test_that('numeric scalar', {
    expect_false(stop_condition(0, 1))
    expect_true(stop_condition(0, 0))
    expect_true(stop_condition(1, 0))
})

test_that('numeric vector', {
    expect_false(stop_condition(c(0, 0), c(1, 1)))
    expect_true(stop_condition(c(1, 1), c(1, 1)))
    expect_true(stop_condition(c(1, 1), c(0, 0)))
    expect_true(stop_condition(c(1, 0), c(0, 0)))
})

test_that('named numeric vector', {
    expect_error(stop_condition(c(x=1), c(y=1)))
    expect_true(stop_condition(c(x=1, y=0), c(x=0, y=0)))
    expect_true(stop_condition(c(x=1, y=0), c(y=0, x=0)))
})

test_that('non-ordered scalar', {
    expect_warning(stop_condition(NULL, factor('A')))
})

test_that('ordered scalar', {
    expect_false(stop_condition(ordered('A', levels=LETTERS),
                                ordered('B', levels=LETTERS)))
    expect_true(stop_condition(ordered(c('B'), levels=LETTERS),
                                ordered(c('B'), levels=LETTERS)))
    expect_true(stop_condition(ordered(c('B'), levels=LETTERS),
                                ordered(c('A'), levels=LETTERS)))

})

test_that('ordered vector', {
    expect_false(stop_condition(ordered(c('A', 'A'), levels=LETTERS),
                                ordered(c('B', 'B'), levels=LETTERS)))
    expect_true(stop_condition(ordered(c('B', 'B'), levels=LETTERS),
                                ordered(c('B', 'B'), levels=LETTERS)))
    expect_true(stop_condition(ordered(c('B', 'B'), levels=LETTERS),
                                ordered(c('A', 'A'), levels=LETTERS)))
    expect_true(stop_condition(ordered(c('B', 'A'), levels=LETTERS),
                                ordered(c('A', 'A'), levels=LETTERS)))
})

