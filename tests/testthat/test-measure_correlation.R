# SPDX-FileCopyrightText: 2023 Stephen Wade <stephematician@gmail.com>
# SPDX-License-Identifier: MIT

test_that('get correct range of values for single numeric variable', {
    set.seed(1)
    data <- data.frame(x=rnorm(10L))

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    x_sample_none <- lapply(x_sample_all, '[', 0)
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)
    none_miss <- lapply(lapply(data, length), rep, x=FALSE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_none, x_sample_none, data, none_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )

})

test_that('get correct range of values for single ordered variable', {
    set.seed(1)
    data <- data.frame(
        x=sample(ordered(c('A', 'B', 'C')), size=10L, replace=TRUE)
    )

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    x_sample_none <- lapply(x_sample_all, '[', 0)
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)
    none_miss <- lapply(lapply(data, length), rep, x=FALSE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_none, x_sample_none, data, none_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )

})

test_that('get correct range of values for two ordered variables', {
    set.seed(1)
    data <- data.frame(
        x=sample(ordered(LETTERS[1:3]), size=10L, replace=TRUE),
        y=sample(ordered(letters[1:3]), size=10L, replace=TRUE)
    )

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_all,
                            modifyList(x_sample_all, as.list(data['y'])),
                            data, all_miss),
        0.5 + 0.5 * measure_correlation(
            x_sample_all['y'], as.list(data['y']), data['y'], all_miss['y']
        )
    )

})

test_that('get correct range of values for single categorical variable', {
    set.seed(1)
    data <- data.frame(
        x=sample(factor(LETTERS[1:3]), size=10L, replace=TRUE)
    )

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    x_sample_none <- lapply(x_sample_all, '[', 0)
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)
    none_miss <- lapply(lapply(data, length), rep, x=FALSE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_none, x_sample_none, data, none_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )

})

test_that('get correct range of values for two categorical variables', {
    set.seed(1)
    data <- data.frame(
        x=sample(factor(LETTERS[1:3]), size=10L, replace=TRUE),
        y=sample(factor(letters[1:3]), size=10L, replace=TRUE)
    )

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_all,
                            modifyList(x_sample_all, as.list(data['y'])),
                            data, all_miss),
        0.5 + 0.5 * measure_correlation(
            x_sample_all['y'], as.list(data['y']), data['y'], all_miss['y']
        )
    )

})

test_that('get correct range of values for one ordered and one categorical variable', {
    set.seed(1)
    data <- data.frame(
        x=sample(ordered(LETTERS[1:3]), size=10L, replace=TRUE),
        y=sample(factor(letters[1:3]), size=10L, replace=TRUE)
    )

    x_sample_all <- lapply(data, '[', c(2:10, 1))
    all_miss <- lapply(lapply(data, length), rep, x=TRUE)

    expect_equal(
        measure_correlation(x_sample_all, x_sample_all, data, all_miss),
        c(measure=1)
    )
    expect_lt(
        measure_correlation(x_sample_all, as.list(data), data, all_miss),
        c(measure=1)
    )
    expect_equal(
        measure_correlation(x_sample_all,
                            modifyList(x_sample_all, as.list(data['y'])),
                            data, all_miss),
        0.5 + 0.5 * measure_correlation(
            x_sample_all['y'], as.list(data['y']), data['y'], all_miss['y']
        )
    )
    expect_equal(
        measure_correlation(x_sample_all,
                            modifyList(x_sample_all, as.list(data['x'])),
                            data, all_miss),
        0.5 + 0.5 * measure_correlation(
            x_sample_all['x'], as.list(data['x']), data['x'], all_miss['x']
        )
    )

})

