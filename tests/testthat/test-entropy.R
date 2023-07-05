# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

test_that('get NAN result for empty or NA sample', {
    expect_identical(entropy(NA), 0 / 0)
    expect_identical(entropy(numeric()), 0 / 0)
})

test_that('get zero result for single value ', {

    expect_identical(entropy(letters[1]), 0)

    expect_identical(entropy(factor(letters[1], levels=letters[1:3])), 0)

})

test_that('get exact result for uniform distribution of multiple values', {

    expect_identical(entropy(letters[1:2]), -sum(rep(log2(1 / 2), 2)) / 2)

    expect_identical(entropy(rep(letters[1:3], each=3)),
                     -sum(3 * rep(log2(1 / 3), 3)) / 9)

})

