context('find_most_frequent_values')

test_that('class', {
    X <- data.frame(x=numeric(1),
                    y=integer(1),
                    z=logical(1),
                    u=factor('A', levels=LETTERS[1:3]),
                    v=ordered('A', levels=LETTERS[1:3]))

    expect_silent(find_most_frequent_values(X))
    expect_identical(lapply(X, class),
                     lapply(find_most_frequent_values(X), class))
})

test_that('tibble', {
    X <- tibble::tibble(x=numeric(1),
                        y=integer(1),
                        z=logical(1),
                        u=factor('A', levels=LETTERS[1:3]),
                        v=ordered('A', levels=LETTERS[1:3]))

    expect_silent(find_most_frequent_values(X))
})

test_that('empty data.frame', {
    X <- data.frame(x=numeric(0),
                    y=integer(0),
                    z=logical(0),
                    u=factor(NA, levels=LETTERS[1:3])[0],
                    v=ordered(NA, levels=LETTERS[1:3])[0])

    expect_identical(find_most_frequent_values(X),
                     as.list(X))
})

test_that('empty tibble', {
    X <- tibble::tibble(x=numeric(0),
                        y=integer(0),
                        z=logical(0),
                        u=factor(NA, levels=LETTERS[1:3])[0],
                        v=ordered(NA, levels=LETTERS[1:3])[0])

    expect_identical(find_most_frequent_values(X), as.list(X))
})

test_that('NA column', {
    X <- data.frame(x=rep(NA_real_, 10),
                    y=rep(1, 10))

    expect_identical(find_most_frequent_values(X), list(x=numeric(0), y=1))
})

test_that('single mode', {
    X <- data.frame(x=c(1,1,2,3),
                    y=c(1L,2L,2L,3L),
                    z=c(T,T,T,F),
                    u=factor(c('A', 'B', 'B', 'C'), levels=LETTERS[1:3]),
                    v=ordered(c('A', 'B', 'B', 'C'), levels=LETTERS[1:3]))

    expect_identical(find_most_frequent_values(X),
                     list(x=1,
                          y=2L,
                          z=T,
                          u=factor('B', levels=LETTERS[1:3]),
                          v=ordered('B', levels=LETTERS[1:3])))
})

test_that('two modes', {
    X <- data.frame(x=c(rep(1, 3), rep(2, 3), 3, 3),
                    y=c(rep(1L, 3), 2L, 2L, rep(3L, 3)),
                    z=c(rep(F, 4), rep(T, 4)),
                    u=factor(c(rep('A', 3), rep('B', 3), rep('C', 2)),
                             levels=LETTERS[1:3]),
                    v=ordered(c(rep('A', 3), rep('B', 2), rep('C', 3)),
                              levels=LETTERS[1:3]))

    expect_identical(find_most_frequent_values(X),
                     list(x=c(1, 2),
                          y=c(1L, 3L),
                          z=c(F, T),
                          u=factor(c('A', 'B'), levels=LETTERS[1:3]),
                          v=ordered(c('A', 'C'), levels=LETTERS[1:3])))
})

