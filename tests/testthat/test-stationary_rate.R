context('stationary_rate')

test_that('empty data', {
    expect_equal(1, stationary_rate(list(), list(), data.frame(), list()))
})

test_that('no missing data', {
    expect_equal(1,
                 stationary_rate(list(x=numeric(0)),
                                 list(x=numeric(0)),
                                 data.frame(x=c(1,2,3)),
                                 list(x=c(F,F,F))))
})

test_that('factor', {
    expect_equal(1,
                 stationary_rate(list(x=factor('A', levels=LETTERS)),
                                 list(x=factor('A', levels=LETTERS)),
                                 data.frame(x=factor(LETTERS[1:3],
                                                     levels=LETTERS)),
                                 list(x=c(T,F,F))))
    expect_equal(2/3,
                 stationary_rate(list(x=factor('A', levels=LETTERS)),
                                 list(x=factor('B', levels=LETTERS)),
                                 data.frame(x=factor(LETTERS[1:3],
                                                     levels=LETTERS)),
                                 list(x=c(T,F,F))))
    expect_equal(0,
                 stationary_rate(list(x=factor(LETTERS[1:3], levels=LETTERS)),
                                 list(x=factor(LETTERS[c(3,1,2)],
                                               levels=LETTERS)),
                                 data.frame(x=factor(LETTERS[1:3],
                                                     levels=LETTERS)),
                                 list(x=c(T,T,T))))
})

