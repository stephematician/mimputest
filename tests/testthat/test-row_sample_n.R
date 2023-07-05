# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

test_that('single column', {

    # returns a column of ones
    expect_identical(row_sample_n(1, 1, 10, replace=F),
                     matrix(rep(1L, 10), nrow=10))

    # returns a column
    expect_identical(dim(row_sample_n(10, 1, 10, replace=F)),
                     c(10L, 1L))

    # returns a column
    expect_identical(dim(row_sample_n(10, 1, 100, replace=F)),
                     c(100L, 1L))

})

test_that('empty matrix', {

    expect_identical(dim(row_sample_n(1, 1, 0, replace=F)),
                     c(0L, 0L))

    expect_identical(dim(row_sample_n(1, 1, 0, replace=T)),
                     c(0L, 0L))

})

