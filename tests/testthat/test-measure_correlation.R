context('measure_correlation')

# Tests are somewhat trivial here

test_that('single numeric variable', {
    set.seed(1)
    x_old <- list(x=rnorm(10L))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)])

    expect_equal(measure_correlation(x_old, x_old, data_, list(x=rep(T, 10L))),
                 c(ordered=1))
    expect_equal(measure_correlation(list(x=numeric(0)),
                                     list(x=numeric(0)),
                                     data_,
                                     list(x=rep(F, 10L))),
                 c(ordered=1))
    expect_lt(measure_correlation(x_old,
                                  list(x=data_$x),
                                  data_,
                                  list(x=rep(T, 10L))),
              c(ordered=1))
})

test_that('single ordered variable', {
    set.seed(1)
    x_old <- list(x=sample(ordered(c('A', 'B', 'C')), size=10L, replace=T))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)])

    expect_equal(measure_correlation(x_old, x_old, data_, list(x=rep(T, 10L))),
                 c(ordered=1))
    expect_equal(measure_correlation(list(x=ordered(NULL,
                                                    levels=levels(x_old$x))),
                                     list(x=ordered(NULL,
                                                    levels=levels(x_old$x))),
                                     data_,
                                     list(x=rep(F, 10L))),
                 c(ordered=1))
    expect_lt(measure_correlation(x_old,
                                  list(x=data_$x),
                                  data_,
                                  list(x=rep(T, 10L))),
              c(ordered=1))
})

test_that('two ordered variables', {
    set.seed(1)
    x_old <- list(x=sample(ordered(c('A', 'B', 'C')), size=10L, replace=T),
                  y=sample(ordered(c('a', 'b', 'c')), size=10L, replace=T))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)], y=x_old$y[c(2:10, 1)])

    expect_equal(measure_correlation(x_old,
                                     x_old,
                                     data_,
                                     list(x=rep(T, 10L), y=rep(T,10L))),
                 c(ordered=1))
    expect_lt(measure_correlation(x_old,
                                  list(x=x_old$x, y=data_$y),
                                  data_,
                                  list(x=rep(T, 10L), y=rep(T, 10L))),
              c(ordered=1))
})

test_that('single categorical variable', {
    set.seed(1)
    x_old <- list(x=sample(factor(c('A', 'B', 'C')), size=10L, replace=T))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)])

    expect_equal(measure_correlation(x_old, x_old, data_, list(x=rep(T, 10L))),
                 c(categorical=1))
    expect_equal(measure_correlation(list(x=factor(NULL,
                                                   levels=levels(x_old$x))),
                                     list(x=factor(NULL,
                                                   levels=levels(x_old$x))),
                                     data_,
                                     list(x=rep(F, 10L))),
                 c(categorical=1))
    expect_lt(measure_correlation(x_old,
                                  list(x=data_$x),
                                  data_,
                                  list(x=rep(T, 10L))),
              c(categorical=1))
})

test_that('two categorical variables', {
    set.seed(1)
    x_old <- list(x=sample(factor(c('A', 'B', 'C')), size=10L, replace=T),
                  y=sample(factor(c('a', 'b', 'c')), size=10L, replace=T))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)], y=x_old$y[c(2:10, 1)])

    expect_equal(measure_correlation(x_old,
                                     x_old,
                                     data_,
                                     list(x=rep(T, 10L), y=rep(T, 10L))),
                 c(categorical=1))
    expect_lt(measure_correlation(x_old,
                                  list(x=x_old$x, y=data_$y),
                                  data_,
                                  list(x=rep(T, 10L), y=rep(T, 10L))),
              c(categorical=1))
    expect_equal(measure_correlation(x_old,
                                     list(x=x_old$x, y=data_$y),
                                     data_,
                                     list(x=rep(T, 10L), y=rep(T, 10L))),
                 0.5 + 0.5 * measure_correlation(x_old['y'],
                                                 list(y=data_$y),
                                                 data_['y'],
                                                 list(y=rep(T, 10L))))
})

test_that('ordered and categorical variables', {
    set.seed(1)
    x_old <- list(x=sample(ordered(c('A', 'B', 'C')), size=10L, replace=T),
                  y=sample(factor(c('a', 'b', 'c')), size=10L, replace=T))
    data_ <- data.frame(x=x_old$x[c(2:10, 1)], y=x_old$y[c(2:10, 1)])

    expect_equal(measure_correlation(x_old,
                                     x_old,
                                     data_,
                                     list(x=rep(T, 10L), y=rep(T, 10L))),
                 c(categorical=1, ordered=1))
    expect_equal(measure_correlation(list(x=ordered(NULL,
                                                    levels=levels(x_old$x)),
                                          y=factor(NULL,
                                                   levels=levels(x_old$y))),
                                      list(x=ordered(NULL,
                                                    levels=levels(x_old$x)),
                                           y=factor(NULL,
                                                    levels=levels(x_old$y))),
                                     data_,
                                     list(x=rep(F, 10L), y=rep(F, 10L))),
                 c(categorical=1, ordered=1))
})

