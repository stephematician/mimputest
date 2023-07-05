# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

test_that('class is preserved', {
    data <- data.frame(x=numeric(1), y=integer(1), z=logical(1),
                        u=factor('A', levels=LETTERS[1:3]),
                        v=ordered('A', levels=LETTERS[1:3]))

    expect_silent(lapply(data, most_frequent_values))
    expect_identical(lapply(data, class),
                     lapply(lapply(data, most_frequent_values), class))
})


test_that('get empty vector if sample is empty', {
    data <- data.frame(x=numeric(0), y=integer(0), z=logical(0),
                       u=factor(NA, levels=LETTERS[1:3])[0],
                       v=ordered(NA, levels=LETTERS[1:3])[0])

    expect_identical(lapply(data, most_frequent_values),
                     as.list(data))
})


test_that('get empty vector if all input is NA', {

    data <- data.frame(x=rep(NA_real_, 10), y=rep(NA_integer_, 10),
                       z=rep(NA, 10),
                       u=factor(rep(NA, 10), levels=LETTERS[1:3]),
                       v=ordered(rep(NA, 10), levels=LETTERS[1:3]))

    expect_identical(lapply(data, most_frequent_values), as.list(data[0,]))

})

test_that('get mode of sample with single mode', {
    data <- data.frame(x=c(1,1,2,3), y=c(1L,2L,2L,3L), z=c(rep(TRUE, 3), FALSE),
                       u=factor(c('A', 'B', 'B', 'C'), levels=LETTERS[1:3]),
                       v=ordered(c('A', 'B', 'B', 'C'), levels=LETTERS[1:3]))

    expect_identical(lapply(data, most_frequent_values),
                     list(x=1, y=2L, z=TRUE,
                          u=factor('B', levels=LETTERS[1:3]),
                          v=ordered('B', levels=LETTERS[1:3])))
})

test_that('get modes of sample with two modes', {
    data <- data.frame(x=c(rep(1, 3), rep(2, 3), 3, 3),
                       y=c(rep(1L, 3), 2L, 2L, rep(3L, 3)),
                       z=c(rep(FALSE, 4), rep(TRUE, 4)),
                       u=factor(c(rep('A', 3), rep('B', 3), rep('C', 2)),
                                levels=LETTERS[1:3]),
                       v=ordered(c(rep('A', 3), rep('B', 2), rep('C', 3)),
                                 levels=LETTERS[1:3]))

    expect_identical(lapply(data, most_frequent_values),
                     list(x=c(1, 2), y=c(1L, 3L), z=c(FALSE, TRUE),
                          u=factor(c('A', 'B'), levels=LETTERS[1:3]),
                          v=ordered(c('A', 'C'), levels=LETTERS[1:3])))
})

