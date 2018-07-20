context('no_information_impute')

test_that('empty data.frames', {
    X <- data.frame()
    expect_identical(no_information_impute(X), X)
})

test_that('data.frame attributes', {
    set.seed(1)
    X <- data.frame(x=sample(c(NA_integer_, 0L, 1L), size=10, replace=T),
                    y=sample(c(NA_real_, 0, 1), size=10, replace=T))

    # empty data.frame
    expect_identical(attributes(X[0,]),
                     attributes(no_information_impute(X[0,])))

    # data.frame with NA
    expect_identical(attributes(X),
                     attributes(no_information_impute(X)))
})

test_that('storage modes', {
    set.seed(1)
    X <- data.frame(x=sample(c(NA_integer_, 0L, 1L), size=10, replace=T),
                    y=sample(c(NA_real_, 0, 1), size=10, replace=T),
                    z=sample(c(NA, F, T), size=10, replace=T),
                    u=sample(factor(c('A', 'B', NA)), size=10, replace=T),
                    v=sample(ordered(c('A', 'B', NA)), size=10, replace=T))

    # empty data.frame
    expect_identical(lapply(X[0,], storage.mode),
                     lapply(no_information_impute(X[0,]), storage.mode))

    # data.frame with NA
    expect_identical(lapply(X, storage.mode),
                     lapply(no_information_impute(X), storage.mode))

})

test_that('logical', {
    set.seed(1)
    X <- data.frame(x=sample(c(T,F,NA), size=10, replace=T))

    expect_identical(lapply(X, class),
                     lapply(no_information_impute(X), class))
    expect_true(!any(is.na(no_information_impute(X)$x)))
    expect_true(all(no_information_impute(X)$x %in% c(F,T)))

})

test_that('integers', {
    set.seed(1)
    X <- data.frame(x=rep(c(NA_integer_, 1L), each=5))
    expect_true(!any(is.na(no_information_impute(X)$x)))
    expect_true(all(no_information_impute(X)$x %in% 1L))
})

test_that('factors', {
    set.seed(1)
    X <- data.frame(x=sample(factor(c('A', 'B', 'C', NA)), size=10, replace=T))

    expect_identical(lapply(X, class),
                     lapply(no_information_impute(X), class))

    expect_identical(lapply(X, levels),
                     lapply(no_information_impute(X), levels))
    expect_true(!any(is.na(no_information_impute(X)$x)))
    expect_true(all(no_information_impute(X)$x %in% LETTERS[1:3]))

})

test_that('ordered data', {
    set.seed(1)
    X <- data.frame(x=sample(ordered(c('A', 'B', 'C', NA),
                                     levels=c('B', 'C', 'A')),
                             size=10,
                             replace=T))

    expect_identical(lapply(X, class),
                     lapply(no_information_impute(X), class))
    # data.frame with NA
    expect_identical(lapply(X, levels),
                     lapply(no_information_impute(X), levels))
    expect_true(!any(is.na(no_information_impute(X)$x)))
    expect_true(all(no_information_impute(X)$x %in% LETTERS[1:3]))

})

test_that('tibble attributes', {
    set.seed(1)
    X <- tibble::tibble(x=sample(c(NA_integer_, 0L, 1L), size=10, replace=T),
                        y=sample(c(NA_real_, 0, 1), size=10, replace=T))

    # empty tibble 
    expect_identical(attributes(X[0,]),
                     attributes(no_information_impute(X[0,])))

    # tibble with NA
    expect_identical(attributes(X),
                     attributes(no_information_impute(X)))
})

test_that('data with no missing values', {
    set.seed(1)
    X <- data.frame(x=sample(c(0L, 1L), size=10, replace=T),
                    y=sample(c(0, 1), size=10, replace=T))

    # empty data.frame with no NA
    expect_identical(X[0,], no_information_impute(X[0,]))

    # data.frame with no NA
    expect_identical(X, no_information_impute(X))

})

