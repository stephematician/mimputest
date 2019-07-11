context('measure_degenerate')

# Tests are somewhat trivial here

test_that('trivial limit', {
    j <- NA_integer_
    loop.limit <- 1L

    expect_identical(measure_degenerate(), NULL)
})

test_that('first step', {
    j <- 1L
    loop.limit <- 10L

    expect_identical(measure_degenerate(), c(measure=1L))
})

test_that('second step', {
    j <- 2L
    loop.limit <- 10L

    expect_identical(measure_degenerate(), c(measure=2L))
})

test_that('completed', {
    j <- 10L
    loop.limit <- 10L

    expect_identical(measure_degenerate(), c(measure=0L))
})

