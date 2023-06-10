
test_that('is identity for empty data.frame', {
    data <- data.frame()
    expect_identical(impute_naive_by_aggregate(data), data)
})

test_that('attributes of data.frame are preserved', {
    set.seed(1)
    data <- data.frame(x=sample(c(NA_integer_, 0L, 1L), size=10, replace=TRUE),
                       y=sample(c(NA_real_, 0, 1), size=10, replace=TRUE))

    # empty data.frame
    expect_identical(attributes(data[0,]),
                     attributes(impute_naive_by_aggregate(data[0,])))

    # data.frame with NA
    expect_identical(attributes(data),
                     attributes(impute_naive_by_aggregate(data)))
})

test_that('storage modes are preserved', {
    set.seed(1)
    data <- data.frame(
        x=sample(c(NA_integer_, 0L, 1L), size=10, replace=TRUE),
        y=sample(c(NA_real_, 0, 1), size=10, replace=TRUE),
        z=sample(c(NA, FALSE, TRUE), size=10, replace=TRUE),
        u=sample(factor(c('A', 'B', NA)), size=10, replace=TRUE),
        v=sample(ordered(c('A', 'B', NA)), size=10, replace=TRUE)
    )

    # empty data.frame
    expect_identical(lapply(data[0,], storage.mode),
                     lapply(impute_naive_by_aggregate(data[0,]), storage.mode))

    # data.frame with NA
    expect_identical(lapply(data, storage.mode),
                     lapply(impute_naive_by_aggregate(data), storage.mode))

})

test_that('can impute logical value', {
    set.seed(1)
    data <- data.frame(x=sample(c(TRUE, FALSE, NA), size=10, replace=TRUE))

    expect_identical(lapply(data, class),
                     lapply(impute_naive_by_aggregate(data), class))
    expect_true(!any(is.na(impute_naive_by_aggregate(data)$x)))
    expect_true(all(impute_naive_by_aggregate(data)$x %in% c(FALSE, TRUE)))

})

test_that('can impute integer', {
    set.seed(1)
    data <- data.frame(x=rep(c(NA_integer_, 1L), each=5))
    expect_true(!any(is.na(impute_naive_by_aggregate(data)$x)))
    expect_true(all(impute_naive_by_aggregate(data)$x %in% 1L))
})

test_that('can impute factor', {
    set.seed(1)
    data <- data.frame(
        x=sample(factor(c('A', 'B', 'C', NA)), size=10, replace=TRUE)
    )

    expect_identical(lapply(data, class),
                     lapply(impute_naive_by_aggregate(data), class))

    expect_identical(lapply(data, levels),
                     lapply(impute_naive_by_aggregate(data), levels))
    expect_true(!any(is.na(impute_naive_by_aggregate(data)$x)))
    expect_true(all(impute_naive_by_aggregate(data)$x %in% LETTERS[1:3]))

})

test_that('can impute ordered value', {
    set.seed(1)
    data <- data.frame(
        x=sample(ordered(c('A', 'B', 'C', NA), levels=c('B', 'C', 'A')),
                 size=10,
                 replace=TRUE)
    )

    expect_identical(lapply(data, class),
                     lapply(impute_naive_by_aggregate(data), class))
    # data.frame with NA
    expect_identical(lapply(data, levels),
                     lapply(impute_naive_by_aggregate(data), levels))
    expect_true(!any(is.na(impute_naive_by_aggregate(data)$x)))
    expect_true(all(impute_naive_by_aggregate(data)$x %in% LETTERS[1:3]))

})

test_that('attributes of tibble are preserved', {
    set.seed(1)
    data <- tibble::tibble(
        x=sample(c(NA_integer_, 0L, 1L), size=10, replace=TRUE),
        y=sample(c(NA_real_, 0, 1), size=10, replace=TRUE)
    )

    # empty tibble
    expect_identical(attributes(data[0,]),
                     attributes(impute_naive_by_aggregate(data[0,])))

    # tibble with NA
    expect_identical(attributes(data),
                     attributes(impute_naive_by_aggregate(data)))
})

test_that('is identity for data with no missing values', {
    set.seed(1)
    data <- data.frame(x=sample(c(0L, 1L), size=10, replace=TRUE),
                       y=sample(c(0, 1), size=10, replace=TRUE))

    # empty data.frame with no NA
    expect_identical(data[0,], impute_naive_by_aggregate(data[0,]))

    # data.frame with no NA
    expect_identical(data, impute_naive_by_aggregate(data))

})

