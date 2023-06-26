# Copyright (c) Cancer Council NSW, 2018-2023. All rights reserved.

test_that('get NAN result for empty or NA sample', {
    expect_identical(leiks_D(NA), 0 / 0)
    expect_identical(leiks_D(numeric()), 0 / 0)
})

test_that('missing levels are preserved', {

    expect_identical(leiks_D(factor(letters[1], levels=letters[1:3])), 0)

})

test_that('get zero result for single value', {

    expect_identical(leiks_D(ordered(letters[1])), 0)

    expect_identical(leiks_D(ordered(letters[1], levels=letters[1:3])), 0)

})

test_that('get exact result for uniform distribution of multiple values', {

    expect_identical(leiks_D(factor(letters[1:2])), 2 * sum(c(1) / 2) / 1)

    expect_identical(leiks_D(factor(rep(letters[1:3], each=3))),
                     2 * sum(c(3, 9-6) / 9) / 2)

})

