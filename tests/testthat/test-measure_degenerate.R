# Copyright (c) Cancer Council NSW, 2018-2023. All rights reserved.

test_that('always null if stop on first step', {
    j <- NA_integer_
    loop_limit <- 1L
    expect_identical(measure_degenerate(), NULL)
})

test_that('returns number of steps when not at (non-trivial) limit', {
    j <- 1L
    loop_limit <- 10L
    expect_identical(measure_degenerate(), c(measure=1L))
    j <- 2L
    expect_identical(measure_degenerate(), c(measure=2L))
})

test_that('returns zero when reached non-trivial limit', {
    j <- 10L
    loop_limit <- 10L
    expect_identical(measure_degenerate(), c(measure=0L))
})

